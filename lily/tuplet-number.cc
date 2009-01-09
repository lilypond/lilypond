/*
  tuplet-number.cc -- implement Tuplet_number

  source file of the GNU LilyPond music typesetter

  (c) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "tuplet-bracket.hh"
#include "moment.hh"
#include "paper-column.hh"
#include "text-interface.hh"
#include "spanner.hh"
#include "lookup.hh"

struct Tuplet_number
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_GROB_INTERFACE ();
};
  

MAKE_SCHEME_CALLBACK (Tuplet_number, print, 1);
SCM 
Tuplet_number::print (SCM smob)
{
  Spanner *me = unsmob_spanner (smob);
  Spanner *tuplet = unsmob_spanner (me->get_object ("bracket")); 

  if (!tuplet || !tuplet->is_live ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  SCM stc_scm = Text_interface::print (smob);
  Stencil *stc = unsmob_stencil (stc_scm);

  stc->align_to (X_AXIS, CENTER);
  stc->align_to (Y_AXIS, CENTER);

  SCM cpoints =  tuplet->get_property ("control-points");
  Drul_array<Offset> points;
  if (scm_is_pair (cpoints))
    {
      points[LEFT] = ly_scm2offset (scm_car (cpoints));
      points[RIGHT] = ly_scm2offset (scm_cadr (cpoints));
    }
  else
    {
      programming_error ("wrong type for control-points");
    }
  stc->translate ((points[RIGHT] + points[LEFT]) / 2);
  
  return stc_scm;
}

MAKE_SCHEME_CALLBACK (Tuplet_number, calc_cross_staff, 1)
SCM
Tuplet_number::calc_cross_staff (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  return unsmob_grob (me->get_object ("bracket"))->get_property ("cross-staff");
}

ADD_INTERFACE (Tuplet_number,
	       "The number for a bracket.",

	       /* properties */
	       "avoid-slur " 	// UGH.
	       "bracket "
	       );

