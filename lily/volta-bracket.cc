/*
  volta-bracket.cc -- implement Volta_bracket_interface

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <cstring>
using namespace std;

#include "warn.hh"
#include "font-interface.hh"
#include "line-interface.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "text-interface.hh"
#include "volta-bracket.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "directional-element-interface.hh"
#include "lookup.hh"
#include "tuplet-bracket.hh"

/*
  this is too complicated. Yet another version of side-positioning,
  badly implemented.

  --

  * Should look for system_start_delim to find left edge of staff.
  */

MAKE_SCHEME_CALLBACK (Volta_bracket_interface, print, 1);
SCM
Volta_bracket_interface::print (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);
  Spanner *orig_span = dynamic_cast<Spanner *> (me->original ());
  bool broken_first_bracket = orig_span && (orig_span->broken_intos_[0]
					    == (Spanner *)me);
  
  Output_def *layout = me->layout ();
  Real half_space = 0.5;

  Item *bound = dynamic_cast<Spanner *> (me)->get_bound (LEFT);

  /*
    not a start, but really broken in two
  */
  Real left = 0.;
  if (bound->break_status_dir () == RIGHT)
    {
      Paper_column *pc = bound->get_column ();
      left = pc->extent (pc, X_AXIS)[RIGHT]
	- bound->relative_coordinate (pc, X_AXIS);
    }
  else
    {
      /*
	the volta spanner is attached to the bar-line, which is moved
	to the right. We don't need to compensate for the left edge.
      */
    }

  modify_edge_height (me);
  if (!me->is_live ())
    return SCM_EOL;
  
  Drul_array<Real> edge_height = robust_scm2interval (me->get_property ("edge-height"),
						      Interval (1.0, 1.0));
  Drul_array<Real> flare = robust_scm2interval (me->get_property ("bracket-flare"),
						Interval (0, 0));
  Drul_array<Real> shorten = robust_scm2interval (me->get_property ("shorten-pair"),
						  Interval (0, 0));


  
  scale_drul (&edge_height, - Real (get_grob_direction (me)));

  Interval empty;
  Offset start;
  start[X_AXIS] = me->spanner_length () - left - half_space;
  
  /*
    ugh, Tuplet_bracket should use Horizontal_bracket, not the other way around. 
  */
  Stencil total
    = Tuplet_bracket::make_bracket (me, Y_AXIS, start, 
				    edge_height, empty, flare, shorten);

  if (!orig_span || broken_first_bracket)
    {
      SCM text = me->get_property ("text");
      SCM properties = me->get_property_alist_chain (SCM_EOL);
      SCM snum = Text_interface::interpret_markup (layout->self_scm (),
						   properties, text);
      Stencil num = *unsmob_stencil (snum);
      num.align_to (Y_AXIS, UP);
      num.translate_axis (-0.5, Y_AXIS);
      total.add_at_edge (X_AXIS, LEFT, num, - num.extent (X_AXIS).length ()
			 - 1.0);
    }
  
  total.translate_axis (left, X_AXIS);
  return total.smobbed_copy ();
}


void
Volta_bracket_interface::modify_edge_height (Spanner *me)
{
  Spanner *orig_span = dynamic_cast<Spanner *> (me->original ());
 
  bool broken_first_bracket = orig_span && (orig_span->broken_intos_[0] == (Spanner *)me);
  bool broken_last_bracket = orig_span && (orig_span->broken_intos_.back () == (Spanner *)me);
  bool no_vertical_start = orig_span && !broken_first_bracket;
  bool no_vertical_end = orig_span && !broken_last_bracket;

  extract_grob_set (me, "bars", bars);
  Grob *endbar = bars.size () ? bars.back () : 0;
  SCM glyph = endbar ? endbar->get_property ("glyph-name") : SCM_EOL;

  string str;
  if (scm_is_string (glyph))
    str = ly_scm2string (glyph);
  else
    str = "|";

  no_vertical_end
    |= (str != ":|"
	&& str != "|:"
	&& str != "|."
	&& str != ":|:"
	&& str != ":|.|:"
	&& str != ":|.:"
	&& str != ".|");

  if (no_vertical_end || no_vertical_start)
    {
      Drul_array<Real> edge_height = robust_scm2interval (me->get_property ("edge-height"),
							  Interval (1.0, 1.0));
      if (no_vertical_start)
	edge_height[LEFT] = 0.0;

      if (no_vertical_end)
	edge_height[RIGHT] = 0.0;

      me->set_property ("edge-height", ly_interval2scm (edge_height));
    }

  if (broken_last_bracket && no_vertical_end && no_vertical_start
      && !broken_first_bracket)
    me->suicide ();
}

void
Volta_bracket_interface::add_bar (Grob *me, Item *b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("bars"), b);
  add_bound_item (dynamic_cast<Spanner *> (me), b);
}

ADD_INTERFACE (Volta_bracket_interface,
	       "Volta bracket with number.",

	       /* properties */
	       "bars "
	       "thickness "
	       "height "
	       );

