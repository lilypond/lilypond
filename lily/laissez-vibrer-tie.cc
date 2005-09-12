/*
  laissez-vibrer-tie.cc -- implement Laissez_vibrer_tie

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "laissez-vibrer-tie-column.hh"
#include "laissez-vibrer-tie.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "tie.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"

ADD_INTERFACE(Laissez_vibrer_tie,
	      "laissez-vibrer-tie-interface",
	      "The interface for l.v. tie items.",

	      /* properties */
	      "control-points "
	      "details "
	      "thickness "
	      "x-gap "
	      "details "
	      "note-head "
	      );

MAKE_SCHEME_CALLBACK (Laissez_vibrer_tie, print, 1);
SCM
Laissez_vibrer_tie::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (CENTER == get_grob_direction (me))
    set_direction (me);

  if (!get_grob_direction (me))
    me->programming_error ("lv. tie direction not set."); 
    
  SCM cp = me->get_property ("control-points");
  if (!scm_is_pair (cp))
    if (Laissez_vibrer_tie_column::has_interface (me->get_parent (Y_AXIS)))
      {
	Laissez_vibrer_tie_column::set_directions (me->get_parent (Y_AXIS));
      }

  return Tie::print (smob);
}

void
Laissez_vibrer_tie::set_direction (Grob *me)
{
  if (!get_grob_direction (me))
    {
      if (Laissez_vibrer_tie_column::has_interface (me->get_parent (Y_AXIS)))
	Laissez_vibrer_tie_column::set_directions (me->get_parent (Y_AXIS));
      else
	{
	  programming_error ("lv tie without Laissez_vibrer_tie_column"); 
	  set_grob_direction (me, UP);
	}
    }
}

int
Laissez_vibrer_tie::get_position (Grob *me)
{
  Grob *h = unsmob_grob (me->get_object ("note-head"));
  return (int) rint (Staff_symbol_referencer::get_position (h));
}

int
Laissez_vibrer_tie::compare (Grob *const &s1,
			     Grob *const &s2)
{
  return sign (get_position (s1) - get_position (s2));
}
				 
