/*
  plet-spanner.cc -- implement Tuplet_bracket

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>

#include "beam.hh"
#include "box.hh"
#include "debug.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "text-item.hh"
#include "tuplet-spanner.hh"
#include "stem.hh"
#include "note-column.hh"
#include "dimensions.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"

/*
  TODO:
  staff-line collisions for horizontal tuplet brackets.
 */



void
Tuplet_bracket::set_interface (Grob*me)
{
  me->set_interface (ly_symbol2scm ("tuplet-bracket"));
}

/*
  TODO: use stem->beam fields to find Beams. Autobeams aren't found
  through the engraver mechanism.  */
MAKE_SCHEME_CALLBACK (Tuplet_bracket,brew_molecule,1);
SCM
Tuplet_bracket::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Molecule  mol;
  Link_array<Grob> column_arr=
    Pointer_group_interface__extract_elements (me, (Grob*)0, "columns");


  if (!column_arr.size ())
    return mol.smobbed_copy ();


  Grob *b1 = Note_column::stem_l (column_arr[0]); 
  Grob *b2 = Note_column::stem_l (column_arr.top());    

  b1 = b1 ? Stem::beam_l (b1) : 0;
  b2 = b2 ? Stem::beam_l (b2) : 0;

  
  Spanner*sp = dynamic_cast<Spanner*> (me);  

  // Default behaviour: number always, bracket when no beam!
  bool par_beam = b1 && (b1 == b2) && !sp->broken_b() ;
  
  bool bracket_visibility = !par_beam;
  bool number_visibility = true;

  SCM bracket = me->get_grob_property ("tuplet-bracket-visibility");
  if (gh_boolean_p (bracket))
    {
      bracket_visibility = gh_scm2bool (bracket);
    }
  else if (bracket == ly_symbol2scm ("if-no-beam"))
    bracket_visibility = !par_beam;

  SCM numb = me->get_grob_property ("tuplet-number-visibility");  
  if (gh_boolean_p (numb))
    {
      number_visibility = gh_scm2bool (numb);
    }
  else if (bracket == ly_symbol2scm ("if-no-beam"))
    number_visibility = !par_beam;
  
	
  Real ncw = column_arr.top ()->extent (column_arr.top (), X_AXIS).length ();
  Real w = sp->spanner_length () + ncw;

  Direction dir = Directional_element_interface::get (me);
  Real dy = gh_scm2double (me->get_grob_property ("delta-y"));
  SCM number = me->get_grob_property ("text");
  if (gh_string_p (number) && number_visibility)
    {
      SCM properties = Font_interface::font_alist_chain (me);
      Molecule num = Text_item::text2molecule (me, number, properties);
      num.align_to (X_AXIS, CENTER);
      num.translate_axis (w/2, X_AXIS);
      num.align_to (Y_AXIS, CENTER);
	
      num.translate_axis (dy/2, Y_AXIS);

      mol.add_molecule (num);
    }
      
  if (bracket_visibility)      
    {
      Real  lt =  me->paper_l ()->get_var ("stafflinethickness");
	  
      SCM thick = me->get_grob_property ("thick");
      SCM gap = me->get_grob_property ("number-gap");
	  
      SCM at =gh_list (ly_symbol2scm ("tuplet"),
		       gh_double2scm (1.0),
		       gap,
		       gh_double2scm (w),
		       gh_double2scm (dy),
		       gh_double2scm (gh_scm2double (thick)* lt),
		       gh_int2scm (dir),
		       SCM_UNDEFINED);

      Box b;
      mol.add_molecule (Molecule (b, at));
    }

  return mol.smobbed_copy ();
}




/*
  use first -> last note for slope, and then correct for disturbing
  notes in between.  */
void
Tuplet_bracket::calc_position_and_height (Grob*me,Real *offset, Real * dy) 
{
  Link_array<Grob> column_arr=
    Pointer_group_interface__extract_elements (me, (Grob*)0, "columns");


  Grob * commony = me->common_refpoint (me->get_grob_property ("columns"), Y_AXIS);
  Grob * commonx = me->common_refpoint (me->get_grob_property ("columns"), X_AXIS);  
  
  Direction d = Directional_element_interface::get (me);

  /*
    Use outer non-rest columns to determine slope
   */
  int l = 0;
  while (l <column_arr.size () && Note_column::rest_b (column_arr[l]))
    l ++;

  int r = column_arr.size ()- 1;
  while (r >= l && Note_column::rest_b (column_arr[r]))
    r--;
  
  if (l < r)
    {
      *dy = column_arr[r]->extent (commony, Y_AXIS) [d]
	- column_arr[l]->extent (commony, Y_AXIS) [d] ;
    }
  else
    * dy = 0;


  *offset = - d * infinity_f;

  if (!column_arr.size ())
    return;
  
  Real x0 = column_arr[0]->relative_coordinate (commonx, X_AXIS);
  Real x1 = column_arr.top ()->relative_coordinate (commonx, X_AXIS);
  
  Real factor = column_arr.size () > 1 ? 1/ (x1 - x0) : 1.0;
  
  for (int i = 0; i < column_arr.size ();  i++)
    {
      Real notey = column_arr[i]->extent (commony, Y_AXIS)[d] 
	- me->relative_coordinate (commony, Y_AXIS);

      Real x = column_arr[i]->relative_coordinate (commonx, X_AXIS) - x0;
      Real tuplety =  *dy * x * factor;

      if (notey * d > (*offset + tuplety) * d)
	*offset = notey - tuplety; 
    }

  // padding
  *offset +=  1.0 *d;

  
  /*
    horizontal brackets should not collide with staff lines.

    
   */
  if (*dy == 0)
    {
      // quantize, then do collision check.
      Real ss= Staff_symbol_referencer::staff_space (me);
      *offset *= 2 / ss;
      
      *offset = rint (*offset);
      if (Staff_symbol_referencer::on_staffline (me, (int) rint (*offset)))
	*offset += d;

      *offset *= 0.5 * ss;
    }
  
}

/*
  use first -> last note for slope,
*/
void
Tuplet_bracket::calc_dy (Grob*me,Real * dy)
{
  Link_array<Grob> column_arr=
    Pointer_group_interface__extract_elements (me, (Grob*)0, "columns");

  /*
    ugh. refps.
   */
  Direction d = Directional_element_interface::get (me);
  *dy = column_arr.top ()->extent (column_arr.top (), Y_AXIS) [d]
    - column_arr[0]->extent (column_arr[0], Y_AXIS) [d];
}
MAKE_SCHEME_CALLBACK (Tuplet_bracket,after_line_breaking,1);

SCM
Tuplet_bracket::after_line_breaking (SCM smob)
{
  Grob * me = unsmob_grob (smob);
  Link_array<Note_column> column_arr=
    Pointer_group_interface__extract_elements (me, (Note_column*)0, "columns");

  if (!column_arr.size ())
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }

  Direction d = Directional_element_interface::get (me);
  if (!d)
    {
      d = Tuplet_bracket::get_default_dir (me);
      Directional_element_interface::set (me, d);

    }
  Real dy, offset;

  calc_position_and_height (me,&offset,&dy);
  
  me->set_grob_property ("delta-y", gh_double2scm (dy));

  me->translate_axis (offset, Y_AXIS);
  return SCM_UNSPECIFIED;
}


Direction
Tuplet_bracket::get_default_dir (Grob*me)
{
  Direction d = UP;
  SCM dir_sym =me->get_grob_property ("dir-forced");
  if (isdir_b (dir_sym))
    {
      d= to_dir (dir_sym);
      if (d != CENTER)
	return d;
    }

  d = UP ;
  for (SCM s = me->get_grob_property ("columns"); gh_pair_p (s); s = gh_cdr (s))
    {
      Grob * nc = unsmob_grob (gh_car (s));
      if (Note_column::dir (nc) < 0) 
	{
	  d = DOWN;
	  break;
	}
    }
  
  return d;
}

void
Tuplet_bracket::add_column (Grob*me, Item*n)
{
  Pointer_group_interface::add_element (me, "columns",n);
  me->add_dependency (n);

  add_bound_item (dynamic_cast<Spanner*> (me), n);
}


