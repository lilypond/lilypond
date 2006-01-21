/*
  laissez-vibrer-tie.cc -- implement Laissez_vibrer_tie

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
	      "direction "
	      "details "
	      "note-head "
	      "thickness "
	      );

MAKE_SCHEME_CALLBACK(Laissez_vibrer_tie, calc_control_points, 1)
SCM
Laissez_vibrer_tie::calc_control_points (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (Laissez_vibrer_tie_column::has_interface (me->get_parent (Y_AXIS)))
    {
      me->get_parent (Y_AXIS)->get_property ("positioning-done");
    }
  else
    {
      programming_error ("lv tie without Laissez_vibrer_tie_column. Killing lv tie."); 
      me->suicide (); 
    }
  
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK(Laissez_vibrer_tie, calc_direction, 1)
SCM
Laissez_vibrer_tie::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (Laissez_vibrer_tie_column::has_interface (me->get_parent (Y_AXIS)))
    me->get_parent (Y_AXIS)->get_property("positioning-done");
  else
    {
      programming_error ("lv tie without Laissez_vibrer_tie_column"); 
      set_grob_direction (me, UP);
    }

  return SCM_UNSPECIFIED;
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
				 
