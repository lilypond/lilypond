/*
  dynamic-text-spanner.cc -- implement Text_spanner

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Jan Nieuwenhuizen <janneke@gnu.org>

  Revised over good by Han-Wen.
*/

#include "text-interface.hh"
#include "text-spanner.hh"
#include "line-spanner.hh"
#include "spanner.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "output-def.hh"
#include "warn.hh"
#include "paper-column.hh"

class Dynamic_text_spanner
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static bool has_interface (Grob *);
};

/*
  This is a partial C&P from text-spanner.cc

  Dynamic_text_spanner is similar, but

  * does not require bracket functionality.

  * should make room for spanning points (mf/f/mp texts).

  * In the future, we should  support

  cresc - - - - poco - - - a - - - - poco - - -

  as well


  The cut & paste is rather inelegant, but text-spanner was a failed
  and buggy attempt at being generic.
*/
MAKE_SCHEME_CALLBACK (Dynamic_text_spanner, print, 1);
SCM
Dynamic_text_spanner::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Spanner *spanner = dynamic_cast<Spanner *> (me);

  Grob *common = spanner->get_bound (LEFT)
    ->common_refpoint (spanner->get_bound (RIGHT), X_AXIS);
  Output_def *layout = me->layout ();

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
	  Real pad = 0.0;
	  Real encl = d;
	  if (b->internal_has_interface (ly_symbol2scm ("dynamic-interface")))
	    {
	      pad = robust_scm2double (me->get_property ("bound-padding"), 0.0);
	      encl = -d;
	    }

	  /*
	    ugh - a special case.

	    FIXME: this fixed an issue, but need to have this issue in
	    the issue tracker.

	    This fix doesn't quite work: it should only do this if the
	    right bound has a trill too.
	   */
#if 0
	  if (d == RIGHT && me->get_property ("style") == ly_symbol2scm ("trill"))
	    {
	      pad = 2.0;
	      encl = LEFT;
	    }
#endif
	  
	  Interval ext = b->extent (common, X_AXIS);
	  span_points[d] = -d * pad
	    + robust_relative_extent (b, common, X_AXIS)
	    .linear_combination (encl);
	}
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

  Stencil m;
  do
    {
      Interval ext = edge[d].extent (X_AXIS);
      Real pad = robust_scm2double (me->get_property ("bound-padding"), 0.0);
      if (!ext.is_empty ())
	{
	  edge[d].translate_axis (span_points[d], X_AXIS);
	  m.add_stencil (edge[d]);
	  span_points[d] += -d * (ext[-d] + pad);
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

ADD_INTERFACE (Dynamic_text_spanner,
	       "dynamic-text-spanner-interface",
	       "A text spanner for crescendo texts",
	       
	       "bound-padding "
	       "dash-period "
	       "dash-fraction "
	       "edge-text "
	       "style "
	       "thickness");

