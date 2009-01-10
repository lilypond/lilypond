/*
  semi-tie.cc -- implement Semi_tie

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "semi-tie-column.hh"
#include "semi-tie.hh"
#include "directional-element-interface.hh"
#include "grob.hh"
#include "tie.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"

ADD_INTERFACE (Semi_tie,
	      "A tie which is only on one side connected to a note head.",

	      /* properties */
	      "control-points "
	      "direction "
	      "details "
	      "head-direction "
	      "note-head "
	      "thickness "
	      );

MAKE_SCHEME_CALLBACK (Semi_tie, calc_control_points, 1)
SCM
Semi_tie::calc_control_points (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  (void) me->get_property ("direction");
  
  if (Semi_tie_column::has_interface (me->get_parent (Y_AXIS)))
    {
      me->get_parent (Y_AXIS)->get_property ("positioning-done");
    }
  else
    {
      programming_error ("lv tie without Semi_tie_column. Killing lv tie."); 
      me->suicide (); 
    }

  return me->get_property_data ("control-points");
}

int
Semi_tie::get_position (Grob *me)
{
  Grob *h = unsmob_grob (me->get_object ("note-head"));
  return (int) rint (Staff_symbol_referencer::get_position (h));
}

bool
Semi_tie::less (Grob *const &s1,
		Grob *const &s2)
{
  return get_position (s1) < get_position (s2);
}
				 
