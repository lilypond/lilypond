/*
  volta-spanner.cc -- implement Volta_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "box.hh"
#include "debug.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "volta-spanner.hh"
#include "group-interface.hh"
#include "side-position-interface.hh"
#include "directional-element-interface.hh"


void
Volta_spanner::set_interface (Score_element*me)
{
  me->set_elt_property ("bars", SCM_EOL);
  Side_position::set_axis (me, Y_AXIS);
  Directional_element_interface (me).set (UP);
}


/*
  this is too complicated. Yet another version of side-positioning,
  badly implemented.

  --

  * Should look for system_start_delim to find left edge of staff.
  
  
*/

MAKE_SCHEME_CALLBACK(Volta_spanner,brew_molecule);
SCM
Volta_spanner::brew_molecule (SCM smob) 
{
  Score_element *me = unsmob_element (smob);
  Link_array<Item> bar_arr
    = Pointer_group_interface__extract_elements (me, (Item*)0, "bars");

  if (!bar_arr.size ())
    return SCM_EOL;

  bool no_vertical_start = false;
  bool no_vertical_end = to_boolean (me->get_elt_property ("last-volta"));
  Spanner *orig_span =  dynamic_cast<Spanner*> (me->original_l_);
  if (orig_span && (orig_span->broken_into_l_arr_[0] != (Spanner*)me))
    no_vertical_start = true;
  if (orig_span && (orig_span->broken_into_l_arr_.top () != (Spanner*)me))
    no_vertical_end = true;

#if 0
  // FIXME
  if (bar_arr.top ()->me->get_elt_property (type_str_.length_i () > 1)
    no_vertical_end = false;
#endif

  Real staff_space = me->paper_l ()->get_var ("interline");
  Real half_space = staff_space / 2;
  Real left = dynamic_cast<Spanner*>(me)->get_broken_left_end_align ();
  Real w = dynamic_cast<Spanner*>(me)->spanner_length () - left - half_space;
  Real h = me->paper_l()->get_var ("volta_spanner_height");
  Real t = me->paper_l ()->get_var ("volta_thick");

  /*
    ugh: should build from line segments.
   */
  SCM at = (gh_list (ly_symbol2scm ("volta"),
		     gh_double2scm (h),
		     gh_double2scm (w),
		     gh_double2scm (t),
		     gh_int2scm (no_vertical_start),
		     gh_int2scm (no_vertical_end),
		     SCM_UNDEFINED));

  Box b (Interval (0, w), Interval (0, h));
  Molecule  mol (b, at);
  Molecule num (me->lookup_l ()->text ("volta",
				       ly_scm2string (me->get_elt_property("text")),
				       me->paper_l ()));

  mol.add_at_edge (X_AXIS, LEFT, num, - num.extent (X_AXIS).length ()
		   - staff_space);
  mol.translate_axis (left, X_AXIS);
  return mol.create_scheme();
}


void
Volta_spanner::add_bar  (Score_element *me, Item* b)
{
  Pointer_group_interface gi(me, "bars");
  gi.add_element (b);

  Side_position::add_support (me,b);
  add_bound_item (dynamic_cast<Spanner*>(me), b); 
}

void
Volta_spanner::add_column (Score_element*me, Score_element* c)
{
  Side_position::add_support (me,c);
}
