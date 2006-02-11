/*
  horizontal-bracket.cc -- implement Horizontal_bracket

  source file of the GNU LilyPond music typesetter

  (c) 2002--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "horizontal-bracket.hh"	

#include "lookup.hh"
#include "side-position-interface.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "tuplet-bracket.hh"
#include "axis-group-interface.hh"


Stencil
Horizontal_bracket::make_bracket (Grob *me,
				  Real length,
				  Axis a, Direction dir)				 
{
  Drul_array<Real> edge_height = robust_scm2interval (me->get_property ("edge-height"),
						      Interval (1.0, 1.0));
  Drul_array<Real> flare = robust_scm2interval (me->get_property ("bracket-flare"),
						Interval (0, 0));
  Drul_array<Real> shorten = robust_scm2interval (me->get_property ("shorten-pair"),
						  Interval (0, 0));

  // Make sure that it points in the correct direction:
  scale_drul (&edge_height, Real (-dir));
 
  Interval empty;
  Offset start;
  start[a] = length;

  /*
    ugh, Tuplet_bracket should use Horizontal_bracket, not the other way around. 
  */
  return Tuplet_bracket::make_bracket (me, other_axis (a), start, 
				       edge_height, empty, flare, shorten);
}


Stencil
Horizontal_bracket::make_enclosing_bracket (Grob *me, Grob *refpoint,
					    vector<Grob*> grobs,
					    Axis a, Direction dir)
{
  Grob *common = common_refpoint_of_array (grobs, refpoint, a);
  Interval ext = Axis_group_interface::relative_group_extent (grobs, common, a);

  Stencil b = make_bracket (me, ext.length(), a, dir);
  b.translate_axis (ext[LEFT] - refpoint->relative_coordinate (common, a), a);

  return b;
}


/*
  TODO:

  Support texts on the brackets?
*/

MAKE_SCHEME_CALLBACK (Horizontal_bracket, print, 1);
SCM
Horizontal_bracket::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "columns", gs);
  if (!gs.size ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Stencil b = make_enclosing_bracket (me, me, gs, X_AXIS, get_grob_direction (me));
  return b.smobbed_copy ();
}

ADD_INTERFACE (Horizontal_bracket,

	       "horizontal-bracket-interface",
	       "A horizontal bracket encompassing notes.",

	       /* props */		  
	       "columns "
	       "bracket-flare "
	       "shorten-pair "
	       "edge-height");

