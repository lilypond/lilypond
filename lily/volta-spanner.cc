/*
  volta-spanner.cc -- implement Volta_spanner

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <string.h>

#include "box.hh"
#include "debug.hh"
#include "font-interface.hh"
#include "molecule.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "text-item.hh"
#include "volta-spanner.hh"
#include "group-interface.hh"
#include "side-position-interface.hh"
#include "directional-element-interface.hh"


void
Volta_spanner::set_interface (Grob*)
{
}

/*
  this is too complicated. Yet another version of side-positioning,
  badly implemented.

  --

  * Should look for system_start_delim to find left edge of staff.
  
*/

MAKE_SCHEME_CALLBACK (Volta_spanner,brew_molecule,1);
SCM
Volta_spanner::brew_molecule (SCM smob) 
{
  Grob *me = unsmob_grob (smob);
  Link_array<Item> bar_arr
    = Pointer_group_interface__extract_grobs (me, (Item*)0, "bars");

  if (!bar_arr.size ())
    return SCM_EOL;

  Spanner *orig_span =  dynamic_cast<Spanner*> (me->original_l_);

  bool first_bracket = orig_span && (orig_span->broken_into_l_arr_[0] == (Spanner*)me);
  
  bool last_bracket = orig_span && (orig_span->broken_into_l_arr_.top () == (Spanner*)me);

  bool no_vertical_start = orig_span && !first_bracket;
  bool no_vertical_end = orig_span && !last_bracket;
  SCM bars = me->get_grob_property ("bars");
  Grob * endbar =   unsmob_grob (ly_car (bars));
  SCM glyph = endbar->get_grob_property("glyph");
  String str = ly_scm2string(glyph);
  const char* cs = str.ch_C();
  no_vertical_end |=
    (strcmp(cs,":|")!=0 && strcmp(cs,"|:")!=0 && strcmp(cs,"|.")!=0
     && strcmp(cs,":|:")!=0 && strcmp(cs,".|")!=0);

  Real staff_thick = me->paper_l ()->get_var ("stafflinethickness");  
  Real half_space = 0.5;

  Item * bound = dynamic_cast<Spanner*> (me)->get_bound (LEFT);

  /*
    not a start, but really broken in two
   */
  Real left =0.;  
  if (bound->break_status_dir () == RIGHT)
  {
    Paper_column *pc = bound->column_l ();
    left = pc->extent (pc, X_AXIS)[RIGHT]   - bound->relative_coordinate (pc, X_AXIS);
  }
  else
  {
    /*
      the volta spanner is attached to the bar-line, which is moved
      to the right. We don't need to compensate for the left edge.
    */
  }

  Real w = dynamic_cast<Spanner*> (me)->spanner_length () - left - half_space;
  Real h =  gh_scm2double (me->get_grob_property ("height"));
  Real t =  staff_thick * gh_scm2double (me->get_grob_property ("thickness"));

  /*
    ugh: should build from line segments.
   */
  SCM at = (scm_list_n (ly_symbol2scm ("volta"),
		     gh_double2scm (h),
		     gh_double2scm (w),
		     gh_double2scm (t),
		     gh_int2scm (no_vertical_start),
		     gh_int2scm (no_vertical_end),
		     SCM_UNDEFINED));

  Box b (Interval (0, w), Interval (0, h));
  Molecule mol (b, at);
  SCM text = me->get_grob_property ("text");
  SCM properties = scm_list_n (me->mutable_property_alist_, me->immutable_property_alist_,SCM_UNDEFINED);
  Molecule num = Text_item::text2molecule (me, text, properties);

  mol.add_at_edge (X_AXIS, LEFT, num, - num.extent (X_AXIS).length ()
		   - 1.0);
  mol.translate_axis (left, X_AXIS);
  return mol.smobbed_copy ();
}


void
Volta_spanner::add_bar (Grob *me, Item* b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("bars"), b);
  Side_position_interface::add_support (me,b);
  add_bound_item (dynamic_cast<Spanner*> (me), b); 
}

void
Volta_spanner::add_column (Grob*me, Grob* c)
{
  Side_position_interface::add_support (me,c);
}
