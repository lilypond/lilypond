/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "span-bar.hh"

#include "font-interface.hh"
#include "dimensions.hh"
#include "output-def.hh"
#include "stencil.hh"
#include "warn.hh"
#include "axis-group-interface.hh"
#include "bar-line.hh"
#include "grob.hh"
#include "pointer-group-interface.hh"

void
Span_bar::add_bar (Grob *me, Grob *b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), b);
}

MAKE_SCHEME_CALLBACK (Span_bar, print, 1);

/* Limitations/Bugs:

(1) Elements from 'me->get_object ("elements")' must be
ordered according to their y coordinates relative to their common
axis group parent.  Otherwise, the computation goes mad.

(2) This method depends on bar_engraver not being removed from
staff context.  If bar_engraver is removed, the size of the staff
lines is evaluated as 0, which results in a solid span bar line
with faulty y coordinate. */

/* This routine was originally by Juergen Reuter, but it was a on the
   bulky side. Rewritten by Han-Wen. */
SCM
Span_bar::print (SCM smobbed_me)
{
  Grob *me = unsmob_grob (smobbed_me);
  extract_grob_set (me, "elements", elements);
  Grob *refp = common_refpoint_of_array (elements, me, Y_AXIS);

  SCM glyph = me->get_property ("glyph-name");

  /* glyph may not be a string, when ME is killed by Hara Kiri in
     between. */
  if (!scm_is_string (glyph))
    return SCM_EOL;

  string glyph_string = ly_scm2string (glyph);

  /* compose span_bar_mol */
  vector<Interval> extents;
  vector<bool> make_span_bar;
  Grob *model_bar = 0;
  for (vsize i = elements.size (); i--;)
    {
      Grob *bar = elements[i];
      Interval ext = Bar_line::bar_y_extent (bar, refp);
      if (ext.is_empty ())
	continue;

      extents.push_back (ext);
      make_span_bar.push_back (to_boolean (bar->get_property ("allow-span-bar")));
      model_bar = bar;
    }

  if (!model_bar)
    model_bar = me;

  vector_sort (extents, Interval::left_less);

  Stencil span_bar;
  for (vsize i = 1; i < extents.size (); i++)
    {
      Interval prev_extent = extents[i - 1];
      Interval ext = extents[i];
      if (!prev_extent.is_empty ())
	{
	  Interval l (prev_extent [UP],
		      ext[DOWN]);

	  if (l.is_empty () || !make_span_bar[i])
	    {
	      /* There is overlap between the bar lines.  Do nothing. */
	    }
	  else
	    {
	      Stencil interbar = Bar_line::compound_barline (model_bar,
							     glyph_string,
							     l.length (),
							     false);
	      interbar.translate_axis (l.center (), Y_AXIS);
	      span_bar.add_stencil (interbar);
	    }
	}
      prev_extent = ext;
    }

  span_bar.translate_axis (- me->relative_coordinate (refp, Y_AXIS),
			   Y_AXIS);

  return span_bar.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Span_bar, width, 1);
SCM
Span_bar::width (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  SCM gn = me->get_property ("glyph-name");
  if (!me->is_live ())
    return ly_interval2scm (Interval ());
  
  string gl = ly_scm2string (gn);

  /*
    urg.
  */
  Stencil m = Bar_line::compound_barline (me, gl, 40 PT, false);

  return ly_interval2scm (m.extent (X_AXIS));
}

MAKE_SCHEME_CALLBACK (Span_bar, before_line_breaking, 1);
SCM
Span_bar::before_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "elements", elements);
  if (elements.empty ())
    me->suicide ();

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Span_bar, center_on_spanned_callback, 1);

SCM
Span_bar::center_on_spanned_callback (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Interval i (get_spanned_interval (me));

  /* Bar_line::print delivers a barline of y-extent (-h/2, h/2), so
     we have to translate ourselves to be in the center of the
     interval that we span. */
  if (i.is_empty ())
    {
      me->suicide ();
      return scm_from_double (0.0);
    }

  return scm_from_double (i.center ());
}



MAKE_SCHEME_CALLBACK(Span_bar, calc_glyph_name, 1);
SCM
Span_bar::calc_glyph_name (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "elements", elements);
  SCM gl = SCM_EOL;
  for (vsize i = elements.size ();
       i-- && !scm_is_string (gl);)
    gl = elements[i]->get_property ("glyph-name");

  if (!scm_is_string (gl))
    {
      me->suicide ();
      return SCM_UNSPECIFIED;
    }

  string type = ly_scm2string (gl);
  if (type == "|:")
    type = ".|";
  else if (type == ":|")
    type = "|.";
  else if (type == ":|:")
    type = ".|.";
  else if (type == ":|.|:")
    type = "|.|";
  else if (type == ":|.:")
    type = "|.";
  else if (type == "'")
    type = "";

  return ly_string2scm (type);
}

Interval
Span_bar::get_spanned_interval (Grob *me)
{
  return ly_scm2interval (Axis_group_interface::generic_group_extent (me, Y_AXIS));
}

MAKE_SCHEME_CALLBACK (Span_bar, calc_bar_size, 1);
SCM
Span_bar::calc_bar_size (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Interval iv (get_spanned_interval (me));
  if (iv.is_empty ())
    {
      /* This happens if the bars are hara-kiried from under us. */
      me->suicide ();
      return scm_from_double (-1);
    }
  return scm_from_double (iv.length ());
}

ADD_INTERFACE (Span_bar,
	       "A bar line that is spanned between other barlines.  This"
	       " interface is used for bar lines that connect different"
	       " staves.",

	       /* properties */
	       "glyph-name "
	       "elements "
	       );

