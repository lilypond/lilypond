/*
  volta-spanner.cc -- implement Volta_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "bar.hh"
#include "paper-def.hh"
#include "volta-spanner.hh"
#include "stem.hh"
#include "dimension-cache.hh"
#include "group-interface.hh"



Volta_spanner::Volta_spanner ()
{
  dim_cache_ [Y_AXIS]->set_callback (dim_callback);
  set_elt_property ("bars", SCM_EOL);
  set_elt_property ("note-columns", SCM_EOL);
}

/*
  FIXME: too complicated.
 */
Molecule 
Volta_spanner::do_brew_molecule () const
{
  Molecule  mol;

  Link_array<Bar> bar_arr
    = Group_interface__extract_elements (this, (Bar*)0, "bars");

  if (!bar_arr.size ())
    return mol;

  Link_array<Score_element> note_column_arr
    = Group_interface__extract_elements (this, (Score_element*)0, "note-columns");
  
  bool no_vertical_start = false;
  bool no_vertical_end = to_boolean (get_elt_property ("last-volta"));
  Spanner *orig_span =  dynamic_cast<Spanner*> (original_l_);
  if (orig_span && (orig_span->broken_into_l_arr_[0] != (Spanner*)this))
    no_vertical_start = true;
  if (orig_span && (orig_span->broken_into_l_arr_.top () != (Spanner*)this))
    no_vertical_end = true;

#if 0
  // FIXME
  if (bar_arr.top ()->get_elt_property (type_str_.length_i () > 1)
    no_vertical_end = false;
#endif

  Real staff_space = paper_l ()->get_var ("interline");
  Real half_staff_space = staff_space/2;
  Real t = paper_l ()->get_var ("volta_thick");

  Real dx = half_staff_space;
  Real w = spanner_length() - dx - get_broken_left_end_align ();
  Real h = paper_l()->get_var ("volta_spanner_height");

  SCM at = (gh_list (ly_symbol2scm ("volta"),
		     gh_double2scm (h),
		     gh_double2scm (w),
		     gh_double2scm (t),
		     gh_int2scm (no_vertical_start),
		     gh_int2scm (no_vertical_end),
		     SCM_UNDEFINED));
  Box b (Interval (- h/2, h/2),  Interval (0, w));
  Molecule volta(b,at);
  
  Molecule num (lookup_l ()->text ("volta",
				   ly_scm2string (get_elt_property("text")),
				   paper_l ()));
  Real dy = bar_arr.top ()->extent (Y_AXIS) [UP] >? 
     bar_arr[0]->extent (Y_AXIS) [UP];
  dy += 2 * h;


  /*
    CODE DUPLICATION.
    FIXME (see axis-group-elt, side-pos interface.)
   */
  for (int i = 0; i < note_column_arr.size (); i++)
    dy = dy >? note_column_arr[i]->extent (Y_AXIS)[BIGGER];
  dy -= h;

  Molecule two (lookup_l ()->text ("volta", "2", paper_l ()));
  Real gap = two.dim_.x ().length () / 2;
  Offset off (num.dim_.x ().length () + gap, 
	      h / half_staff_space - gap);
  num.translate (off);
  mol.add_molecule (volta);
  mol.add_molecule (num);
  mol.translate (Offset (0, dy));
  return mol;
}
  
void
Volta_spanner::do_add_processing ()
{

  Link_array<Bar> bar_arr
    = Group_interface__extract_elements (this, (Bar*)0, "bars");

  if (bar_arr.size ())
    {
      set_bound (LEFT, bar_arr[0]);
      set_bound (RIGHT, bar_arr.top ());  
    }
}

/*
    Originally the following comment existed here
    "in most cases, it's a lot better not no have height...",
    but problems existed with collision between volta spanner
    and above staff or lyrics for multi-staff music, so the proper
    height is now being returned. Additional space should still
    be added elsewhere so lyrics from above staff do not sit on
    volta spanner. (Roy R. Rankin)
*/
Interval
Volta_spanner::dim_callback (Dimension_cache const *c)
{
  Volta_spanner * v = dynamic_cast<Volta_spanner*> (c->element_l ());
  Real h = v->paper_l()->get_var ("volta_spanner_height") * 2.;
  return Interval (0., h);
}

void
Volta_spanner::after_line_breaking ()
{
  Link_array<Bar> bar_arr
    = Group_interface__extract_elements (this, (Bar*)0, "bars");
  
  if (bar_arr.size())
    translate_axis (bar_arr[0]->extent (Y_AXIS)[UP], Y_AXIS);
  translate_axis (get_broken_left_end_align (), X_AXIS);
}
  
void
Volta_spanner::add_bar  (Bar* c)
{
  Group_interface gi(this, "bars");
  gi.add_element (c);

  add_dependency (c);
}

void
Volta_spanner::add_column (Note_column* c)
{
  Group_interface gi(this, "note-columns");
  gi.add_element (c);

  add_dependency (c);
}


