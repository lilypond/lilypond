/*
  system-scheme.cc -- implement scheme bindings to System

  source file of the GNU LilyPond music typesetter

  (c) 2007--2008 Joe Neeman <joeneeman@gmail.com>
*/

#include "align-interface.hh"
#include "pointer-group-interface.hh"
#include "system.hh"


LY_DEFINE (ly_system_print, "ly:system-print",
	   1, 0, 0, (SCM system),
	   "Draw the system and return the prob containing its"
	   " stencil.")
{
  Grob *me = unsmob_grob (system);
  System *me_system = dynamic_cast<System*> (me);
  SCM_ASSERT_TYPE (me, system, SCM_ARG1, __FUNCTION__, "grob");

  return me_system->get_paper_system ();
}

LY_DEFINE (ly_system_stretch, "ly:system-stretch",
	   2, 0, 0, (SCM system, SCM amount_scm),
	   "Stretch the system vertically by the given amount."
	   "  This must be called before the system is drawn (for example"
	   " with @code{ly:system-print}).")
{
  Grob *me = unsmob_grob (system);
  Real amount = robust_scm2double (amount_scm, 0.0);
  
  extract_grob_set (me, "elements", elts);
  for (vsize i = 0; i < elts.size (); i++)
    if (Align_interface::has_interface (elts[i]))
      {
	Align_interface::stretch (elts[i], amount, Y_AXIS);
	break;
      }
  return SCM_UNDEFINED;
}
