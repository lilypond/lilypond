/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "clef-item.hh"
#include "string.hh"
#include "molecule.hh"
#include "item.hh"

Clef_item::Clef_item (SCM s)
  : Item (s)
{}


/*
FIXME: should use symbol.

FIXME: this should be schemified.
*/
GLUE_SCORE_ELEMENT(Clef_item,before_line_breaking);
SCM
Clef_item::member_before_line_breaking ()
{
  SCM style_sym =get_elt_property ("style");
  String style;
  if (gh_string_p (style_sym))
    style = ly_scm2string (style_sym);

  SCM glyph = get_elt_property ("glyph");
  
  if (gh_string_p (glyph))
    {
      String s = ly_scm2string (glyph);

      /*
	FIXME: should use fontsize property to set clef changes.
       */
      if (get_elt_property ("non-default") &&
	  break_status_dir() != RIGHT && style != "fullSizeChanges")
	{
	  s += "_change";
	  set_elt_property ("glyph", ly_str02scm (s.ch_C()));	  
	}
    }
  else
    {
      suicide ();
      return SCM_UNDEFINED;
    }

  // ugh.
  /* why not suicide? */
  if (style == "transparent")	// UGH. JUNKME
    {
      set_elt_property ("molecule-callback", SCM_BOOL_T);
      set_extent_callback (0, X_AXIS);
    }

  return SCM_UNDEFINED;
}



