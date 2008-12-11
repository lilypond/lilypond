/*
  ambitus.cc -- implement Ambitus

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "ambitus.hh"

#include "staff-symbol-referencer.hh"
#include "pitch.hh"
#include "note-head.hh"
#include "item.hh"
#include "font-interface.hh"
#include "output-def.hh"
#include "lookup.hh"
#include "pointer-group-interface.hh"

MAKE_SCHEME_CALLBACK (Ambitus, print, 1);
SCM
Ambitus::print (SCM smob)
{
  Item *me = (Item *) unsmob_grob (smob);
  Stencil stencil;

  // FIXME : should be Ambitus_line join heads
  extract_grob_set (me, "note-heads", heads);
  if (to_boolean (me->get_property ("join-heads"))
      && heads.size () > 1)
    {
      Grob *common
	= common_refpoint_of_array (vector<Grob*> (heads.begin (),
						       heads.begin () + 2),
				    me, Y_AXIS);

      Grob *minh = heads[0];
      Grob *maxh = heads[1];

      if (minh->relative_coordinate (common, Y_AXIS)
	  > maxh->relative_coordinate (common, Y_AXIS))
	{
	  Grob *t = maxh;
	  maxh = minh;
	  minh = t;
	}

      Real pad = 0.35;
      Real pmax = maxh->extent (common, Y_AXIS)[DOWN] - pad;
      Real pmin = minh->extent (common, Y_AXIS)[UP] + pad;

      if (pmin < pmax)
	{
	  Real linethickness = me->layout ()->get_dimension (ly_symbol2scm ("line-thickness"))
	    * robust_scm2double (me->get_property ("thickness"), 1.0);
	  Real blotdiameter = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));
	  Interval x_extent = 0.5 * linethickness * Interval (-1, 1);
	  Interval y_extent = Interval (pmin, pmax);
	  Box line_box (x_extent, y_extent);

	  Stencil line = Lookup::round_filled_box (line_box, blotdiameter);
	  line.translate_axis (- me->relative_coordinate (common, Y_AXIS),
			       Y_AXIS);
	  return line.smobbed_copy ();
	}
    }

  return SCM_EOL;
}

ADD_INTERFACE (Ambitus,
	       "The line between note heads for a pitch range.",

	       /* properties */
	       "join-heads "
	       "note-heads "
	       "thickness "
	       );
