/*
  plet-spanner.cc -- implement Tuplet_bracket

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  TODO:

  - tuplet bracket should probably be subject to the same rules as
  beam sloping/quanting.

  - There is no support for kneed brackets, or nested brackets.

  - number placement for parallel beams should be much more advanced:
    for sloped beams some extra horizontal offset must be introduced.

  - number placement is usually done over the center note, not the
    graphical center.
  
 */

#include <math.h>

#include "beam.hh"
#include "box.hh"
#include "warn.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "text-item.hh"
#include "tuplet-bracket.hh"
#include "stem.hh"
#include "note-column.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"


static Grob*
get_x_bound_grob (Grob *g, Direction my_dir)
{
  if (Note_column::get_stem (g)
      && Note_column::dir (g) == my_dir)
    {
      g = Note_column::get_stem (g);
    }
  return g;
}



Grob*
Tuplet_bracket::parallel_beam (Grob *me, Link_array<Grob> const &cols, bool *equally_long)
{
  /*
    ugh: code dup. 
  */
  Grob *s1 = Note_column::get_stem (cols[0]); 
  Grob *s2 = Note_column::get_stem (cols.top());    

  Grob*b1 = s1 ? Stem::get_beam (s1) : 0;
  Grob*b2 = s2 ? Stem::get_beam (s2) : 0;
  
  Spanner*sp = dynamic_cast<Spanner*> (me);  

  *equally_long= false;
  if (! ( b1 && (b1 == b2) && !sp->broken_b() ))
      return 0;

  Link_array<Grob> beam_stems = Pointer_group_interface__extract_grobs
    (b1, (Grob*)0, "stems");

  if (beam_stems.size() == 0)
    {
      programming_error ("Beam under tuplet bracket has no stems!");
      *equally_long = 0;
      return 0;
    }
  
  *equally_long = (beam_stems[0] == s1 && beam_stems.top() == s2);
  return b1;
}


/*
  TODO:

  in the case that there is no bracket, but there is a (single) beam,
  follow beam precisely for determining tuplet number location.
  
 */
MAKE_SCHEME_CALLBACK (Tuplet_bracket,brew_molecule,1);
SCM
Tuplet_bracket::brew_molecule (SCM smob) 
{
  Grob *me= unsmob_grob (smob);
  Molecule  mol;
  Link_array<Grob> columns=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-columns");

  if (!columns.size ())
    return mol.smobbed_copy ();

  bool equally_long = false;
  Grob * par_beam = parallel_beam (me, columns, &equally_long);

  Spanner*sp = dynamic_cast<Spanner*> (me);  

  bool bracket_visibility = !(par_beam && equally_long);
  bool number_visibility = true;

  /*
    Fixme: the type of this prop is sucky.
   */
  SCM bracket = me->get_grob_property ("bracket-visibility");
  if (gh_boolean_p (bracket))
    {
      bracket_visibility = gh_scm2bool (bracket);
    }
  else if (bracket == ly_symbol2scm ("if-no-beam"))
    bracket_visibility = !par_beam;

  SCM numb = me->get_grob_property ("number-visibility");  
  if (gh_boolean_p (numb))
    {
      number_visibility = gh_scm2bool (numb);
    }
  else if (numb == ly_symbol2scm ("if-no-beam"))
    number_visibility = !par_beam;
	
  Grob * commonx = columns[0]->common_refpoint (columns.top (),X_AXIS);

  /*
    Tuplet brackets are normally not broken, but we shouldn't crash if
    they are.
   */
  commonx = commonx->common_refpoint (sp->get_bound(LEFT), X_AXIS);
  commonx = commonx->common_refpoint (sp->get_bound(RIGHT), X_AXIS);  
  
  Direction dir = Directional_element_interface::get (me);

  Grob * lgr = get_x_bound_grob (columns[0], dir);
  Grob * rgr = get_x_bound_grob (columns.top(), dir);  
  Real x0 = lgr->extent (commonx,X_AXIS)[LEFT];
  Real x1 = rgr->extent (commonx,X_AXIS)[RIGHT];

  Real w = x1 -x0;

  Real ly = gh_scm2double (me->get_grob_property ("left-position"));
  Real ry = gh_scm2double (me->get_grob_property ("right-position"));  
  SCM number = me->get_grob_property ("text");
  
  if (gh_string_p (number) && number_visibility)
    {
      SCM properties = Font_interface::font_alist_chain (me);
      Molecule num = Text_item::text2molecule (me, number, properties);
      num.align_to (X_AXIS, CENTER);
      num.translate_axis (w/2, X_AXIS);
      num.align_to (Y_AXIS, CENTER);
	
      num.translate_axis ((ry-ly)/2, Y_AXIS);

      mol.add_molecule (num);
    }
      
  if (bracket_visibility)      
    {
      Real  lt =  me->get_paper ()->get_var ("linethickness");
  
      SCM thick = me->get_grob_property ("thickness");
      if (gh_number_p (thick))
	lt *= gh_scm2double (thick);
      
      SCM gap = me->get_grob_property ("gap");
      SCM ew = me->get_grob_property ("edge-widen");
      SCM eh = me->get_grob_property ("edge-height");
      SCM sp = me->get_grob_property ("shorten-pair");
      
      Direction d = LEFT;
      Drul_array<Real> height, width, shorten;
      do {
	width[d] =  height[d] = shorten[d] = 0.0;
	if ( ly_number_pair_p (ew) )
	  width[d] +=  gh_scm2double (index_get_cell (ew, d));
	if ( ly_number_pair_p (eh) )
	  height[d] += gh_scm2double (index_get_cell (eh, d)) * - dir;
	if ( ly_number_pair_p (sp) )
	  shorten[d] +=  gh_scm2double (index_get_cell (sp, d));
      }
      while (flip (&d) != LEFT);
      
      Molecule brack = make_bracket (Y_AXIS,
				     w, ry - ly, lt,
				     height,
				     gh_scm2double (gap),
				     width,
				     shorten);
      mol.add_molecule (brack);
    }
  
  mol.translate_axis (ly, Y_AXIS);
  mol.translate_axis (x0  - sp->get_bound (LEFT)->relative_coordinate (commonx,X_AXIS),X_AXIS);
  return mol.smobbed_copy ();
}

/*
  should move to lookup?
 */
Molecule
Tuplet_bracket::make_bracket (Axis protusion_axis,
			      Real dx, Real dy, Real thick, Drul_array<Real> height,
			      Real gap,
			      Drul_array<Real> widen,
			      Drul_array<Real> shorten)
{
  Real len = Offset (dx,dy).length ();
  Real gapx = dx * (gap /  len);
  Real gapy = dy * (gap /  len);
  Drul_array<Real> shortx, shorty;
  Direction d = LEFT;
  do {
    shortx[d] = dx * (shorten[d] /  len);
    shorty[d] = dy * (shorten[d] /  len);
  }
  while (flip (&d) != LEFT);
  Axis other = other_axis (protusion_axis);
  
  Molecule l1 = Lookup::line (thick, Offset(shortx[LEFT], shorty[LEFT]),
			      Offset ( (dx - gapx)/2, (dy - gapy)/2 ));

  Molecule l2 = Lookup::line (thick, Offset((dx + gapx) / 2,(dy + gapy) / 2),
			      Offset (dx - shortx[RIGHT], dy - shorty[RIGHT]));

  Offset protusion;
  protusion[other] = -widen[LEFT];
  protusion[protusion_axis] = height[LEFT];
  Molecule p1 = Lookup::line (thick, 
			      Offset(shortx[LEFT], shorty[LEFT]), 
			      Offset(shortx[LEFT], shorty[LEFT]) + protusion);
  protusion[other] = widen[RIGHT];
  protusion[protusion_axis] = height[RIGHT];
  Molecule p2 = Lookup::line (thick, 
			      Offset(dx - shortx[RIGHT], dy - shorty[RIGHT]), 
			      Offset(dx - shortx[RIGHT], dy - shorty[RIGHT]) + protusion);  

  Molecule m;
  m.add_molecule (p1);
  m.add_molecule (p2);
  m.add_molecule (l1);
  m.add_molecule (l2);

  return m;  
}


/*
  use first -> last note for slope, and then correct for disturbing
  notes in between.  */
void
Tuplet_bracket::calc_position_and_height (Grob*me,Real *offset, Real * dy) 
{
  Link_array<Grob> columns=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-columns");


  SCM cols = me->get_grob_property ("note-columns");
  Grob * commony = common_refpoint_of_list (cols, me, Y_AXIS);
  Grob * commonx = common_refpoint_of_list (cols, me, X_AXIS);  
  
  Direction dir = Directional_element_interface::get (me);

  /*
    Use outer non-rest columns to determine slope
   */
  int l = 0;
  while (l <columns.size () && Note_column::rest_b (columns[l]))
    l ++;

  int r = columns.size ()- 1;
  while (r >= l && Note_column::rest_b (columns[r]))
    r--;
  
  if (l < r)
    {
      *dy = columns[r]->extent (commony, Y_AXIS) [dir]
	- columns[l]->extent (commony, Y_AXIS) [dir] ;
    }
  else
    * dy = 0;


  *offset = - dir * infinity_f;

  if (!columns.size ())
    return;


  
  Grob * lgr = get_x_bound_grob (columns[0], dir);
  Grob * rgr = get_x_bound_grob (columns.top(), dir);  
  Real x0 = lgr->extent (commonx,X_AXIS)[LEFT];
  Real x1 = rgr->extent (commonx,X_AXIS)[RIGHT];


    /*
      Slope.
    */
  Real factor = columns.size () > 1 ? 1/ (x1 - x0) : 1.0;
  
  for (int i = 0; i < columns.size ();  i++)
    {
      Real notey = columns[i]->extent (commony, Y_AXIS)[dir] 
	- me->relative_coordinate (commony, Y_AXIS);
      
      Real x = columns[i]->relative_coordinate (commonx, X_AXIS) - x0;
      Real tuplety =  *dy * x * factor;

      if (notey * dir > (*offset + tuplety) * dir)
	*offset = notey - tuplety; 
    }

  // padding
  *offset +=  gh_scm2double (me->get_grob_property ("padding")) *dir;

  
  /*
    horizontal brackets should not collide with staff lines.
    
   */
  Real ss= Staff_symbol_referencer::staff_space (me);
  if (*dy == 0 && fabs (*offset) <  ss * Staff_symbol_referencer::staff_radius (me))
    {
      // quantize, then do collision check.
      *offset *= 2 / ss;
      
      *offset = rint (*offset);
      if (Staff_symbol_referencer::on_staffline (me, (int) rint (*offset)))
	*offset += dir;

      *offset *= 0.5 * ss;
    }
  
}

/*
  use first -> last note for slope,
*/
void
Tuplet_bracket::calc_dy (Grob*me,Real * dy)
{
  Link_array<Grob> columns=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-columns");

  /*
    ugh. refps.
   */
  Direction d = Directional_element_interface::get (me);
  *dy = columns.top ()->extent (columns.top (), Y_AXIS) [d]
    - columns[0]->extent (columns[0], Y_AXIS) [d];
}


/*
  We depend on the beams if there are any.
 */
MAKE_SCHEME_CALLBACK (Tuplet_bracket,before_line_breaking,1);
SCM
Tuplet_bracket::before_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Link_array<Grob> columns=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-columns");


  for (int i = columns.size(); i--;)
    {
      Grob * s =Note_column::get_stem (columns[i]);
      Grob * b = s ? Stem::get_beam (s): 0;
      if (b)
	me->add_dependency (b);
    }
  return SCM_UNDEFINED;
}

MAKE_SCHEME_CALLBACK (Tuplet_bracket,after_line_breaking,1);

SCM
Tuplet_bracket::after_line_breaking (SCM smob)
{
  Grob * me = unsmob_grob (smob);
  Link_array<Grob> columns=
    Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-columns");

  if (!columns.size ())
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }
  if (dynamic_cast<Spanner*> (me)->broken_b ())
    {
      me->warning ( "Tuplet_bracket was across linebreak. Farewell cruel world.");
      me->suicide();
      return SCM_UNSPECIFIED;
    }
  
  Direction dir = Directional_element_interface::get (me);
  if (!dir)
    {
      dir = Tuplet_bracket::get_default_dir (me);
      Directional_element_interface::set (me, dir);
    }
  
  bool equally_long = false;
  Grob * par_beam = parallel_beam (me, columns, &equally_long);

  /*
    We follow the beam only if there is one, and we are next to it.
   */
  Real dy, offset;
  if (!par_beam
      || Directional_element_interface::get (par_beam) != dir)
    {
      calc_position_and_height (me,&offset,&dy);
    }
  else
    {
      SCM ps =  par_beam->get_grob_property ("positions"); 

      Real lp = gh_scm2double (gh_car (ps));
      Real rp = gh_scm2double (gh_cdr (ps));

      /*
	duh. magic.
       */
      offset = lp + dir * (0.5 + gh_scm2double (me->get_grob_property ("padding")));
      dy = rp- lp;
    }
  
  
  SCM lp =  me->get_grob_property ("left-position");
  SCM rp = me->get_grob_property ("right-position");  
  
  if (gh_number_p (lp) && !gh_number_p (rp))
    {
      rp = gh_double2scm (gh_scm2double (lp) + dy);
    }
  else if (gh_number_p (rp) && !gh_number_p (lp))
    {
      lp = gh_double2scm (gh_scm2double (rp) - dy);
    }
  else if (!gh_number_p (rp) && !gh_number_p (lp))
    {
      lp = gh_double2scm (offset);
      rp = gh_double2scm (offset +dy);
    }

  me->set_grob_property ("left-position", lp);
  me->set_grob_property ("right-position", rp);

  return SCM_UNSPECIFIED;
}


/*
  similar to slur.
 */
Direction
Tuplet_bracket::get_default_dir (Grob*me)
{
  Direction d = UP;
  for (SCM s = me->get_grob_property ("note-columns"); gh_pair_p (s); s = ly_cdr (s))
    {
      Grob * nc = unsmob_grob (ly_car (s));
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
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-columns"), n);
  me->add_dependency (n);

  add_bound_item (dynamic_cast<Spanner*> (me), n);
}






ADD_INTERFACE (Tuplet_bracket,"tuplet-bracket-interface",
  "A bracket with a number in the middle, used for tuplets.",
  "note-columns edge-widen edge-height shorten-pair padding gap left-position right-position bracket-visibility number-visibility thickness direction");

