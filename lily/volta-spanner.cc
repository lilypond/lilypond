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
#include "side-position-interface.hh"
#include "directional-element-interface.hh"

Volta_spanner::Volta_spanner (SCM s)
  : Spanner (s)
{
  set_elt_pointer ("bars", SCM_EOL);
  Side_position_interface (this).set_axis (Y_AXIS);
  directional_element (this).set (UP);
}


/*
  this is too complicated. Yet another version of side-positioning,
  badly implemented.

  --

  * Should look for system_start_delim to find left edge of staff.
  
  
*/
Molecule 
Volta_spanner::do_brew_molecule () const
{
  Molecule  mol;

  Link_array<Bar> bar_arr
    = Pointer_group_interface__extract_elements (this, (Bar*)0, "bars");

  if (!bar_arr.size ())
    return mol;

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
  Real half_space = staff_space / 2;

  Real w = spanner_length ()
  - get_broken_left_end_align ()
    - half_space;
  Real h = paper_l()->get_var ("volta_spanner_height");
  Real t = paper_l ()->get_var ("volta_thick");

  SCM at = (gh_list (ly_symbol2scm ("volta"),
		     gh_double2scm (h),
		     gh_double2scm (w),
		     gh_double2scm (t),
		     gh_int2scm (no_vertical_start),
		     gh_int2scm (no_vertical_end),
		     SCM_UNDEFINED));

  Box b (Interval (0, w), Interval (0, h));
  Molecule volta (b, at);
  mol.add_molecule (volta);
  
  Molecule num (lookup_l ()->text ("volta",
				   ly_scm2string (get_elt_property("text")),
				   paper_l ()));

  mol.add_at_edge (X_AXIS, LEFT, num, - num.extent (X_AXIS).length ()
		   - staff_space);
  return mol;
}
  
void
Volta_spanner::do_add_processing ()
{

  Link_array<Bar> bar_arr
    = Pointer_group_interface__extract_elements (this, (Bar*)0, "bars");

  if (bar_arr.size ())
    {
      set_bound (LEFT, bar_arr[0]);
      set_bound (RIGHT, bar_arr.top ());  
    }
}

void
Volta_spanner::after_line_breaking ()
{
  Side_position_interface (this).add_staff_support ();
}
  
void
Volta_spanner::add_bar  (Bar* b)
{
  Pointer_group_interface gi(this, "bars");
  gi.add_element (b);

  Side_position_interface (this).add_support (b);
  add_dependency (b);
}

void
Volta_spanner::add_column (Note_column* c)
{
  Side_position_interface (this).add_support (c);
  add_dependency (c);
}
