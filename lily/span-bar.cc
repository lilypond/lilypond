/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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

  me->add_dependency (b);
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

  Span_bar::evaluate_glyph (me);
  SCM glyph = me->get_property ("glyph");

  /* glyph may not be a string, when ME is killed by Hara Kiri in
     between. */
  if (!scm_is_string (glyph))
    return SCM_EOL;

  String glyph_string = ly_scm2string (glyph);

  /* compose span_bar_mol */
  Array<Interval> extents;
  Grob *model_bar = 0;
  for (int i = elements.size (); i--;)
    {
      Grob *bar = elements[i];
      Interval ext = bar->extent (refp, Y_AXIS);
      if (ext.is_empty ())
	continue;

      extents.push (ext);
      model_bar = bar;
    }

  if (!model_bar)
    model_bar = me;

  extents.sort (&Interval::left_comparison);

  Stencil span_bar;
  for (int i = 1; i < extents.size (); i++)
    {
      Interval prev_extent = extents[i - 1];
      Interval ext = extents[i];
      if (!prev_extent.is_empty ())
	{
	  Interval l (prev_extent [UP],
		      ext[DOWN]);

	  if (l.is_empty ())
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

MAKE_SCHEME_CALLBACK (Span_bar, width_callback, 2);
SCM
Span_bar::width_callback (SCM element_smob, SCM scm_axis)
{
  Grob *se = unsmob_grob (element_smob);
  (void) scm_axis;

  assert ((Axis) scm_to_int (scm_axis) == X_AXIS);
  String gl = ly_scm2string (se->get_property ("glyph"));

  /*
    urg.
  */
  Stencil m = Bar_line::compound_barline (se, gl, 40 PT, false);

  return ly_interval2scm (m.extent (X_AXIS));
}

MAKE_SCHEME_CALLBACK (Span_bar, before_line_breaking, 1);
SCM
Span_bar::before_line_breaking (SCM smob)
{
  Grob *g = unsmob_grob (smob);
  evaluate_empty (g);
  evaluate_glyph (g);

  /* No need to call Bar_line::before_line_breaking (), because the info
     in ELEMENTS already has been procced by
     Bar_line::before_line_breaking (). */
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Span_bar, center_on_spanned_callback, 2);

SCM
Span_bar::center_on_spanned_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  (void) axis;
  assert (scm_to_int (axis) == Y_AXIS);
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

void
Span_bar::evaluate_empty (Grob *me)
{
  /* TODO: filter all hara-kiried out of ELEMENS list, and then
     optionally do suicide. Call this cleanage function from
     center_on_spanned_callback () as well. */

  extract_grob_set (me, "elements", elements);
  if (elements.is_empty ())
    me->suicide ();
}

void
Span_bar::evaluate_glyph (Grob *me)
{
  SCM gl = me->get_property ("glyph");

  if (scm_is_string (gl))
    return;

  extract_grob_set (me, "elements", elements);
  for (int i = elements.size ();
       i-- && !scm_is_string (gl);)
    gl = elements[i]->get_property ("glyph");

  if (!scm_is_string (gl))
    {
      me->suicide ();
      return;
    }

  String type = ly_scm2string (gl);
  if (type == "|:")
    type = ".|";
  else if (type == ":|")
    type = "|.";
  else if (type == ":|:")
    type = ".|.";

  gl = scm_makfrom0str (type.to_str0 ());
  if (scm_equal_p (me->get_property ("glyph"), gl)
      != SCM_BOOL_T)
    me->set_property ("glyph", gl);
}

Interval
Span_bar::get_spanned_interval (Grob *me)
{
  return ly_scm2interval (Axis_group_interface::group_extent_callback
			  (me->self_scm (), scm_from_int (Y_AXIS)));
}

MAKE_SCHEME_CALLBACK (Span_bar, get_bar_size, 1);
SCM
Span_bar::get_bar_size (SCM smob)
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

ADD_INTERFACE (Span_bar, "span-bar-interface",
	       "A bar line that spanned between other barlines. This interface is "
	       " used for  bar lines that connect different staves.",
	       "elements");

