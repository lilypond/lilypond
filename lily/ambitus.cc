/*
  ambitus.cc -- implement Ambitus

  source file of the GNU LilyPond music typesetter

  (c) 2002--2004 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "staff-symbol-referencer.hh"
#include "pitch.hh"
#include "ambitus.hh"
#include "stencil.hh"
#include "note-head.hh"
#include "item.hh"
#include "font-interface.hh"
#include "output-def.hh"
#include "lookup.hh"
#include "group-interface.hh"

MAKE_SCHEME_CALLBACK (Ambitus,print,1);
SCM
Ambitus::print (SCM smob)
{
  Item *me = (Item*) unsmob_grob (smob);
  Stencil stencil;

  // join heads
  Link_array<Grob> heads (Pointer_group_interface__extract_grobs (me, (Grob*)0, "note-heads"));
  if (to_boolean (me->get_property ("join-heads"))
      && heads.size() > 1)
    {
      Grob *common
	= common_refpoint_of_array (heads.slice (0,2), me, Y_AXIS);

      Grob *minh = heads[0];
      Grob *maxh = heads[1];
      
      if (minh->relative_coordinate (common, Y_AXIS) >
	  maxh->relative_coordinate (common, Y_AXIS))
	{
	  maxh = heads[0];
	  minh = heads[0];
	}

      Real pad = 1.35;
      Real pmax = maxh->relative_coordinate (common, Y_AXIS) - pad;
      Real pmin = minh->relative_coordinate (common, Y_AXIS) + pad;
      
      if (pmin < pmax)
	{
	  Real linethickness = me->get_paper ()->get_dimension (ly_symbol2scm ("linethickness"));
	  Real blotdiameter = me->get_paper ()->get_dimension (ly_symbol2scm ("blotdiameter"));
	  Interval x_extent = 0.5 * Interval (-linethickness, +linethickness);
	  Interval y_extent = 0.5 * Interval (pmin, pmax);
	  Box line_box (x_extent, y_extent);

	  Stencil line = Lookup::round_filled_box (line_box, blotdiameter);
	  line.translate_axis (- me-> relative_coordinate (common, Y_AXIS),
				   Y_AXIS);
	  return line.smobbed_copy ();
	}
    }

  return SCM_EOL;
}

ADD_INTERFACE (Ambitus, "ambitus-interface",
  "The line between note heads for a pitch range.",
  "note-heads join-heads");
