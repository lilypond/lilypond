/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "clef-item.hh"
#include "string.hh"
#include "molecule.hh"
#include "item.hh"


/**
  Set a clef in a staff.

  properties:

  non-default -- not set because of existence of a bar?

  change -- is this a change clef (smaller size)?

  glyph -- a string determining what glyph is typeset
  
 */
struct Clef 
{
  static SCM before_line_breaking (SCM);
};


/*
FIXME: should use symbol.

*/
MAKE_SCHEME_SCORE_ELEMENT_CALLBACK(Clef,before_line_breaking);
SCM
Clef::before_line_breaking (SCM smob)
{
  Item * s = dynamic_cast<Item*> (unsmob_element (smob));

  SCM style_sym =s->get_elt_property ("style");
  String style;
  if (gh_string_p (style_sym))
    style = ly_scm2string (style_sym);

  SCM glyph = s->get_elt_property ("glyph");
  
  if (gh_string_p (glyph))
    {
      String str = ly_scm2string (glyph);

      /*
	FIXME: should use fontsize property to set clef changes.
       */
      if (s->get_elt_property ("non-default") &&
	  s->break_status_dir() != RIGHT && style != "fullSizeChanges")
	{
	  str += "_change";
	  s->set_elt_property ("glyph", ly_str02scm (str.ch_C()));	  
	}
    }
  else
    {
      s->suicide ();
      return SCM_UNDEFINED;
    }

  return SCM_UNDEFINED;
}



