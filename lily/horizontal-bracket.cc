/*
  horizontal-bracket.cc -- implement  Horizontal_bracket

  source file of the GNU LilyPond music typesetter

  (c) 2002--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "side-position-interface.hh"
#include "lookup.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "tuplet-bracket.hh"	// ugh.
#include "horizontal-bracket.hh"	// ugh.

/*
  TODO:

  This doesn't look very elegant: should support winged edges.

  Support texts on the brackets?
*/

Stencil
Horizontal_bracket::make_bracket (Grob *me, Grob *common,
				  Link_array<Grob> grobs, Axis a, Direction dir)
{
  Axis other = other_axis (a);
  
  Grob *cx = common_refpoint_of_array (grobs, common, a);

  Interval ext = grobs.top ()->extent (cx, a);
  ext.unite (grobs[0]->extent (cx, a));

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
  start[a] = ext.length ();
  Stencil b
    = Tuplet_bracket::make_bracket (me, other, start, 
				    edge_height, empty, flare, shorten);

  b.translate_axis (ext[LEFT], a);

  return b;
}

MAKE_SCHEME_CALLBACK (Horizontal_bracket, print, 1);
SCM
Horizontal_bracket::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *sp = dynamic_cast<Spanner *> (me);

  extract_grob_set (me, "columns", gs);
  if (!gs.size ())
    {
      me->suicide ();
      return SCM_EOL;
    }

  Grob *cx = me->common_refpoint (sp->get_bound (LEFT), X_AXIS);
  cx = cx->common_refpoint (sp->get_bound (RIGHT), X_AXIS);

  Stencil b = make_bracket (me, cx, gs, X_AXIS, get_grob_direction (me));
 
  b.translate_axis (- sp->get_bound (LEFT)->relative_coordinate (cx, X_AXIS), X_AXIS);

  return b.smobbed_copy ();
}

ADD_INTERFACE (Horizontal_bracket, "horizontal-bracket-interface",
	       "A horizontal bracket encompassing notes.",

	       /* props */		  
	       "columns "
	       "bracket-flare "
	       "shorten-pair "
	       "edge-height");

