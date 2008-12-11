/*
  note-head-scheme.cc -- implement Note_head bindings.

  source file of the GNU LilyPond music typesetter

  (c) 2006--2008 Han-Wen Nienhuys <hanwen@lilypond.org>

*/

#include "note-head.hh"
#include "font-metric.hh"


LY_DEFINE (ly_note_head__stem_attachment, "ly:note-head::stem-attachment",
	  2, 0, 0, (SCM font_metric, SCM glyph_name),
	  "Get attachment in @var{font-metric} for attaching a stem to"
	  " notehead @var{glyph-name}.")
{
  LY_ASSERT_SMOB (Font_metric, font_metric, 1);
  Font_metric *fm = unsmob_metrics (font_metric);
  LY_ASSERT_TYPE (scm_is_string, glyph_name, 2);
  
  return ly_offset2scm (Note_head::get_stem_attachment (fm, ly_scm2string (glyph_name)));
}

