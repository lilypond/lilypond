/*
  horizontal-bracket.cc -- implement  Horizontal_bracket

  source file of the GNU LilyPond music typesetter

  (c) 2002--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "side-position-interface.hh"
#include "lookup.hh"
#include "group-interface.hh"
#include "directional-element-interface.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "tuplet-bracket.hh"	// ugh.

struct Horizontal_bracket
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static bool has_interface (Grob *);
};

/*
  TODO:

  This doesn't look very elegant: should support winged edges.

  Support texts on the brackets?
*/
MAKE_SCHEME_CALLBACK (Horizontal_bracket, print, 1);

SCM
Horizontal_bracket::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *sp = dynamic_cast<Spanner *> (me);
  Link_array<Grob> gs = extract_grob_array (me, ly_symbol2scm ("columns"));

  if (!gs.size ())
    {
      me->suicide ();
      return SCM_EOL;
    }
  Grob *cx = common_refpoint_of_array (gs, me, X_AXIS);
  cx = cx->common_refpoint (sp->get_bound (LEFT), X_AXIS);
  cx = cx->common_refpoint (sp->get_bound (RIGHT), X_AXIS);

  Interval ext = gs.top ()->extent (cx, X_AXIS);
  ext.unite (gs[0]->extent (cx, X_AXIS));

  Drul_array<Real> edge_height = robust_scm2interval (me->get_property ("edge-height"),
						      Interval (1.0, 1.0));

  Drul_array<Real> flare = robust_scm2interval (me->get_property ("bracket-flare"),
						Interval (0, 0));

  Drul_array<Real> shorten = robust_scm2interval (me->get_property ("shorten-pair"),
						  Interval (0, 0));

  Interval empty;
  Stencil b
    = Tuplet_bracket::make_bracket (me, Y_AXIS, Offset (ext.length (), 0),
				    edge_height, empty, flare, shorten);

  b.translate_axis (ext[LEFT] - sp->get_bound (LEFT)->relative_coordinate (cx, X_AXIS), X_AXIS);

  return b.smobbed_copy ();
}

ADD_INTERFACE (Horizontal_bracket, "horizontal-bracket-interface",
	       "A horizontal bracket encompassing notes.",
	       "columns bracket-flare shorten-pair edge-height");

