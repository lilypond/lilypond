/*
  volta-bracket.cc -- implement Volta_bracket_interface

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <string.h>

#include "box.hh"
#include "warn.hh"
#include "font-interface.hh"
#include "line-interface.hh"
#include "stencil.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "text-item.hh"
#include "volta-bracket.hh"
#include "group-interface.hh"
#include "side-position-interface.hh"
#include "directional-element-interface.hh"
#include "lookup.hh"

/*
  this is too complicated. Yet another version of side-positioning,
  badly implemented.

  --

  * Should look for system_start_delim to find left edge of staff.
  
*/

MAKE_SCHEME_CALLBACK (Volta_bracket_interface,print,1);
SCM
Volta_bracket_interface::print (SCM smob) 
{
  Grob *me = unsmob_grob (smob);
  Spanner *orig_span =  dynamic_cast<Spanner*> (me->original_);

  bool broken_first_bracket = orig_span && (orig_span->broken_intos_[0] == (Spanner*)me);

  bool broken_last_bracket = orig_span && (orig_span->broken_intos_.top () == (Spanner*)me);

  bool no_vertical_start = orig_span && !broken_first_bracket;
  bool no_vertical_end = orig_span && !broken_last_bracket;
  SCM s = me->get_property ("bars");

  Grob * endbar = gh_pair_p (s) ?  unsmob_grob (ly_car (s)) : 0;
  SCM glyph = endbar ? endbar->get_property ("glyph") : SCM_EOL;
  
  String str;
  if (gh_string_p (glyph))
    str = ly_scm2string (glyph);
  else
    str = "|";
  
  const char* cs = str.to_str0 ();
  no_vertical_end |=
    (strcmp (cs,":|")!=0 && strcmp (cs,"|:")!=0 && strcmp (cs,"|.")!=0
     && strcmp (cs,":|:")!=0 && strcmp (cs,".|")!=0);

  Paper_def * paper =me->get_paper ();
  Real half_space = 0.5;

  Item * bound = dynamic_cast<Spanner*> (me)->get_bound (LEFT);

  /*
    not a start, but really broken in two
  */
  Real left =0.;  
  if (bound->break_status_dir () == RIGHT)
    {
      Paper_column *pc = bound->get_column ();
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
  Real h =  robust_scm2double (me->get_property ("height"), 1);

  Stencil start,end ;
  if (!no_vertical_start)
    start = Line_interface::line (me, Offset (0,0), Offset (0, h)); 
  
  if (!no_vertical_end)
    end = Line_interface::line (me, Offset (w, 0), Offset (w,h));

  Stencil mol = Line_interface::line (me, Offset (0, h), Offset (w,h));
  mol.add_stencil (start);
  mol.add_stencil (end);

  if (!orig_span || broken_first_bracket)
    {
      SCM text = me->get_property ("text");
      SCM properties = me->get_property_alist_chain (SCM_EOL);
      SCM snum  = Text_item::interpret_markup (paper->self_scm (), properties, text);
      Stencil num = *unsmob_stencil (snum);

      mol.add_at_edge (X_AXIS, LEFT, num, - num.extent (X_AXIS).length ()
		       - 1.0, 0);
    }
  mol.translate_axis (left, X_AXIS);
  return mol.smobbed_copy ();
}


void
Volta_bracket_interface::add_bar (Grob *me, Item* b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("bars"), b);
  Side_position_interface::add_support (me,b);
  add_bound_item (dynamic_cast<Spanner*> (me), b); 
}

void
Volta_bracket_interface::add_column (Grob*me, Grob* c)
{
  Side_position_interface::add_support (me,c);
}

ADD_INTERFACE (Volta_bracket_interface,"volta-bracket-interface",
	       "Volta bracket with number",
	       "bars thickness height");

