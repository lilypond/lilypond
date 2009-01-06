/*
  font-interface.cc -- implement Font_interface

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "font-interface.hh"

#include "all-font-metrics.hh"
#include "output-def.hh"
#include "warn.hh"
#include "grob.hh"

/* todo: split up this func, reuse in text_item?  */
Font_metric *
Font_interface::get_default_font (Grob *me)
{
  Font_metric *fm = unsmob_metrics (me->get_property ("font"));
  if (!fm)
    {
      SCM chain = music_font_alist_chain (me);

      fm = select_font (me->layout (), chain);
      me->set_property ("font", fm->self_scm ());
    }

  return fm;
}

SCM
Font_interface::music_font_alist_chain (Grob *g)
{
  SCM defaults
    = g->layout ()->lookup_variable (ly_symbol2scm ("font-defaults"));
  if (defaults == SCM_UNDEFINED)
    defaults = SCM_EOL;
  return g->get_property_alist_chain (defaults);
}

SCM
Font_interface::text_font_alist_chain (Grob *g)
{
  SCM defaults
    = g->layout ()->lookup_variable (ly_symbol2scm ("text-font-defaults"));
  if (defaults == SCM_UNDEFINED)
    defaults = SCM_EOL;
  return g->get_property_alist_chain (defaults);
}

ADD_INTERFACE (Font_interface,
	       "Any symbol that is typeset through fixed sets of glyphs,"
	       " (i.e., fonts).",

	       /* properties */
	       "font "
	       "font-encoding "
	       "font-family "
	       "font-name "
	       "font-series "
	       "font-shape "
	       "font-size "
	       );
