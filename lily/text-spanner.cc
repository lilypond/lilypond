/*
  text-spanner.cc -- implement Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Jan Nieuwenhuizen <janneke@gnu.org>

  Revised over good by Han-Wen.
*/

#include "text-spanner.hh"

#include "text-interface.hh"
#include "line-spanner.hh"
#include "spanner.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "output-def.hh"
#include "warn.hh"
#include "paper-column.hh"
#include "staff-symbol-referencer.hh"

/*
  TODO:
  - vertical start / vertical end (fixme-name) |
  - contination types (vert. star, vert. end)  |-> eat volta-bracket
  - more styles
  - more texts/positions
*/

MAKE_SCHEME_CALLBACK (Text_spanner, print, 1);

/*
  TODO: this function is too long
*/
SCM
Text_spanner::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner *> (me);

  /* Ugh, must be same as Hairpin::print.  */

  Grob *common = spanner->get_bound (LEFT)->common_refpoint (spanner->get_bound (RIGHT), X_AXIS);
  Output_def *layout = me->layout ();

  SCM flare = me->get_property ("bracket-flare");
  SCM shorten = me->get_property ("shorten-pair");

  Interval span_points;
  Drul_array<bool> broken;
  Direction d = LEFT;
  do
    {
      Item *b = spanner->get_bound (d);
      broken[d] = b->break_status_dir () != CENTER;

      if (broken[d])
	{
	  if (d == LEFT)
	    span_points[d] = spanner->get_broken_left_end_align ();
	  else
	    span_points[d] = b->relative_coordinate (common, X_AXIS);
	}
      else
	{
	  Real encl = robust_scm2double (me->get_property ("enclose-bounds"), 0.0);
	  Interval ext = b->extent (common, X_AXIS);

	  span_points[d]
	    = robust_relative_extent (b, common, X_AXIS).linear_combination (d * encl);

	  if (is_number_pair (shorten))
	    span_points -= d * scm_to_double (index_get_cell (shorten, d));
	}

      if (is_number_pair (flare))
	span_points -= d * scm_to_double (index_get_cell (flare, d));
    }
  while (flip (&d) != LEFT);

  SCM properties = Font_interface::text_font_alist_chain (me);
  SCM edge_text = me->get_property ("edge-text");
  Drul_array<Stencil> edge;
  if (scm_is_pair (edge_text))
    {
      Direction d = LEFT;
      do
	{
	  if (broken[d])
	    continue;

	  SCM text = index_get_cell (edge_text, d);

	  if (Text_interface::is_markup (text))
	    edge[d] = *unsmob_stencil (Text_interface::interpret_markup (layout->self_scm (), properties, text));

	  if (!edge[d].is_empty ())
	    edge[d].align_to (Y_AXIS, CENTER);
	}
      while (flip (&d) != LEFT);
    }

  Drul_array<Real> edge_height = robust_scm2interval (me->get_property ("edge-height"),
						      Interval (0.0, 0.0));
  Drul_array<Stencil> edge_line;
  {
    Direction d = LEFT;
    int dir = to_dir (me->get_property ("direction"));
    do
      {
	if (broken[d])
	  continue;

	Real dx = 0.0;
	if (is_number_pair (flare))
	  dx = scm_to_double (index_get_cell (flare, d)) * d;

	Real dy = -dir * edge_height[d];
	if (dy)
	  edge_line[d] = Line_spanner::line_stencil (me, Offset (0, 0), Offset (dx, dy));
      }
    while (flip (&d) != LEFT);
  }

  Stencil m;
  do
    {
      Interval ext = edge[d].extent (X_AXIS);
      if (!ext.is_empty ())
	{
	  Real pad = robust_scm2double (me->get_property ("bound-padding"), 0.0);
	  edge[d].translate_axis (span_points[d], X_AXIS);
	  m.add_stencil (edge[d]);
	  span_points[d] += -d * (ext[-d] + pad);
	}
    }
  while (flip (&d) != LEFT);

  do
    {
      if (d * span_points[d] > d * edge[-d].extent (X_AXIS)[d])
	{
	  edge_line[d].translate_axis (span_points[d], X_AXIS);
	  m.add_stencil (edge_line[d]);
	}
    }
  while (flip (&d) != LEFT);

  
  if (!span_points.is_empty ()
      && span_points.length () > robust_scm2double (me->get_property ("dash-period"), 0.0))
    {
      Stencil l = Line_spanner::line_stencil (me,
					      Offset (span_points[LEFT], 0),
					      Offset (span_points[RIGHT], 0));
      m.add_stencil (l);
    }
  m.translate_axis (- me->relative_coordinate (common, X_AXIS), X_AXIS);
  return m.smobbed_copy ();
}

ADD_INTERFACE (Text_spanner,
	       "text-spanner-interface",

	       "generic text spanner",

	       "bound-padding "
	       "bracket-flare "
	       "dash-fraction "
	       "dash-period "
	       "edge-height "
	       "edge-text "
	       "enclose-bounds "
	       "shorten-pair "
	       "style "
	       "thickness "
	       );

