/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "clef-item.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "text-item.hh"
#include "paper-score.hh"
#include "dimension-cache.hh"
#include "side-position-interface.hh"
#include "warn.hh"
#include "line-of-score.hh"

void
Clef_item::before_line_breaking ()
{
  SCM style_sym =get_elt_property ("style");
  String style;
  if (gh_string_p (style_sym))
    style = ly_scm2string (style_sym);

  SCM glyph = get_elt_property ("glyph");
  if (gh_string_p (glyph))
    {
      String s = ly_scm2string (glyph);
	
      if (break_status_dir() != RIGHT && style != "fullSizeChanges")
	{
	  s += "_change";
	}
      s = "clefs-" +  s;
      set_elt_property ("glyph", ly_str02scm (s.ch_C()));
    }
  else
    {
      set_elt_property ("transparent", SCM_BOOL_T);
    }
  
  if (style == "transparent")	// UGH. JUNKME
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_extent_callback (0, X_AXIS);
    }
}



