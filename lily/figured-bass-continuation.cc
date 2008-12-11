/*
  figured-bass-continuation.cc -- implement Figured_bass_continuation

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "line-interface.hh"
#include "spanner.hh"
#include "output-def.hh"
#include "item.hh"
#include "stencil.hh"
#include "pointer-group-interface.hh"
#include "axis-group-interface.hh"


#include "horizontal-bracket.hh"

struct Figured_bass_continuation
{
  DECLARE_GROB_INTERFACE ();
  
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (center_on_figures, (SCM));
};

MAKE_SCHEME_CALLBACK (Figured_bass_continuation, center_on_figures, 1);
SCM
Figured_bass_continuation::center_on_figures (SCM grob)
{
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (grob));
  extract_grob_set (me, "figures", figures);
  if (figures.empty ())
    return scm_from_double (0.0);
  Grob *common = common_refpoint_of_array (figures, me, Y_AXIS);

  Interval ext = Axis_group_interface::relative_group_extent (figures, common, Y_AXIS);
  if (ext.is_empty ())
    return scm_from_double (0.0);
  return scm_from_double (ext.center () - me->relative_coordinate (common, Y_AXIS));
}

MAKE_SCHEME_CALLBACK (Figured_bass_continuation, print, 1);
SCM
Figured_bass_continuation::print (SCM grob)
{
  Spanner *me = dynamic_cast<Spanner*> (unsmob_grob (grob));

  Real thick =
    me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"))
    * robust_scm2double (me->get_property ("thickness"), 1);
  
  Interval spanned;
  Direction d = LEFT;
  Grob *common = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT),
							X_AXIS);
  do
    {
      Item *bound = me->get_bound (d);
      Direction extdir =
	(d == LEFT && to_boolean (bound->get_property ("implicit")))
	? LEFT : RIGHT;

      spanned[d] 
	= robust_relative_extent (bound, common, X_AXIS)[extdir]
	- me->relative_coordinate (common, X_AXIS);
    }
  while (flip (&d) !=  LEFT);
  spanned.widen (- robust_scm2double (me->get_property ("padding"), 0.2));
  
  Stencil extender;
  if (!spanned.is_empty ())
    extender = Line_interface::make_line (thick,
					  Offset (spanned[LEFT], 0),
					  Offset (spanned[RIGHT], 0));
  
  return extender.smobbed_copy ();
}

ADD_INTERFACE (Figured_bass_continuation,
	      "Simple extender line between bounds.",
	      
	      /* properties */
	      "thickness "
	      "padding "
	      "figures "
	      );
	      
