/*
  tuplet-number.cc -- implement Tuplet_number

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "tuplet-bracket.hh"
#include "text-interface.hh"
#include "spanner.hh"
#include "lookup.hh"

struct Tuplet_number
{
  DECLARE_SCHEME_CALLBACK(print, (SCM));
  static bool has_interface (Grob *);
};
  

MAKE_SCHEME_CALLBACK(Tuplet_number, print, 1);
SCM 
Tuplet_number::print (SCM smob)
{
  Stencil *stc = unsmob_stencil (Text_interface::print (smob));

  stc->align_to (X_AXIS, CENTER);
  stc->align_to (Y_AXIS, CENTER);

  Spanner *me = unsmob_spanner (smob);
  Spanner *tuplet = unsmob_spanner (me->get_object ("bracket")); 
  SCM cpoints =  tuplet->get_property ("control-points");
  Drul_array<Offset> points;
  points[LEFT] = ly_scm2offset (scm_car (cpoints));
  points[RIGHT] = ly_scm2offset (scm_cadr (cpoints));

  stc->translate ((points[RIGHT] + points[LEFT]) / 2);
  
  return stc->smobbed_copy ();
}


ADD_INTERFACE (Tuplet_number,
	       "tuplet-number-interface",
	       "The number for a bracket. "
	       ,

	       /* properties */
	       "avoid-slur " 	// UGH.
	       "bracket ");

