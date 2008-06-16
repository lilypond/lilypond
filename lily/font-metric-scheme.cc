/*
  font-metric-scheme.cc -- implement Font_metric scheme bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "font-metric.hh"

#include "warn.hh"
#include "stencil.hh"
#include "modified-font-metric.hh"

LY_DEFINE (ly_font_get_glyph, "ly:font-get-glyph",
	   2, 0, 0,
	   (SCM font, SCM name),
	   "Return a stencil from @var{font} for the glyph named @var{name}."
	   " If the glyph is not available, return an empty stencil.")
{
  Font_metric *fm = unsmob_metrics (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_string, name, 2);

  Stencil m = fm->find_by_name (ly_scm2string (name));

  /* TODO: make optional argument for default if not found.  */
  return m.smobbed_copy ();
}

LY_DEFINE (ly_get_glyph, "ly:get-glyph",
	   2, 0, 0,
	   (SCM font, SCM index),
	   "Retrieve a stencil for the glyph numbered @var{index}"
	   " in @var{font}.")
{
  Font_metric *fm = unsmob_metrics (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_number, index,2);

  return fm->get_ascii_char_stencil (scm_to_int (index)).smobbed_copy ();
}

LY_DEFINE (ly_font_glyph_name_to_index, "ly:font-glyph-name-to-index",
	   2, 0, 0,
	   (SCM font, SCM name),
	   "Return the index for @var{name} in @var{font}.")
{
  Font_metric *fm = unsmob_metrics (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_string, name, 2);

  return scm_from_int (fm->name_to_index (ly_scm2string (name)));
}

LY_DEFINE (ly_font_index_to_charcode, "ly:font-index-to-charcode",
	   2, 0, 0,
	   (SCM font, SCM index),
	   "Return the character code for @var{index} in @var{font}.")
{
  Font_metric *fm = unsmob_metrics (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_integer, index, 2);

  return scm_from_unsigned_integer (fm->index_to_charcode (scm_to_int (index)));
}

LY_DEFINE (ly_font_glyph_name_to_charcode, "ly:font-glyph-name-to-charcode",
	   2, 0, 0,
	   (SCM font, SCM name),
	   "Return the character code for glyph @var{name} in @var{font}.")
{
  Font_metric *fm = unsmob_metrics (font);
  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_string, name, 2);

  return scm_from_unsigned_integer (fm->index_to_charcode (fm->name_to_index (ly_scm2string (name))));
}

LY_DEFINE (ly_text_dimension, "ly:text-dimension",
	   2, 0, 0,
	   (SCM font, SCM text),
	   "Given the font metric in @var{font} and the string @var{text},"
	   " compute the extents of that text in that font.  The return"
	   " value is a pair of number-pairs.")
{
  Box b;
  Modified_font_metric *fm = dynamic_cast<Modified_font_metric *>
    (unsmob_metrics (font));

  LY_ASSERT_SMOB (Font_metric, font, 1);
  LY_ASSERT_TYPE (scm_is_string, text, 2);
  Stencil stc (fm->text_stencil (ly_scm2string (text)));
  return scm_cons (ly_interval2scm (stc.extent (X_AXIS)),
		   ly_interval2scm (stc.extent (Y_AXIS)));
}


/*
  TODO: when are non string retvals allowed?
 */
LY_DEFINE (ly_font_file_name, "ly:font-file-name",
	   1, 0, 0,
	   (SCM font),
	   "Given the font metric @var{font},"
	   " return the corresponding file name.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);

  Font_metric *fm = unsmob_metrics (font);
  SCM name = fm->font_file_name ();

  return name;
}

LY_DEFINE (ly_font_name, "ly:font-name",
	   1, 0, 0,
	   (SCM font),
	   "Given the font metric @var{font},"
	   " return the corresponding name.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);
  Font_metric *fm = unsmob_metrics (font);

  return ly_string2scm (fm->font_name ());
}

LY_DEFINE (ly_font_magnification, "ly:font-magnification", 1, 0, 0,
	   (SCM font),
	   "Given the font metric @var{font}, return the"
	   " magnification, relative to the current output-scale.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);

  Font_metric *fm = unsmob_metrics (font);
  return scm_cdr (fm->description_);
}

LY_DEFINE (ly_font_design_size, "ly:font-design-size", 1, 0, 0,
	   (SCM font),
	   "Given the font metric @var{font}, return the"
	   " design size, relative to the current output-scale.")
{
  LY_ASSERT_SMOB (Font_metric, font, 1);

  Font_metric *fm = unsmob_metrics (font);
  return scm_from_double (fm->design_size ());
}

