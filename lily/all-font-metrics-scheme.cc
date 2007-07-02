/* 
  all-font-metrics-scheme.cc -- implement bindings for
  All_font_metrics.
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2007 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "all-font-metrics.hh"
#include "main.hh"

LY_DEFINE (ly_reset_all_fonts, "ly:reset-all-fonts", 0, 0, 0,
	   (),
	   "Forget all about previously loaded fonts.")
{
  delete all_fonts_global;
  all_fonts_global = new All_font_metrics (global_path.to_string ());

  return SCM_UNSPECIFIED;
}


LY_DEFINE (ly_font_load, "ly:font-load", 1, 0, 0,
	   (SCM name),
	   "Load the font @var{name}.")
{
  LY_ASSERT_TYPE (scm_is_string, name, 1);
  
  Font_metric *fm = all_fonts_global->find_font (ly_scm2string (name));

  return fm->self_scm ();
}


