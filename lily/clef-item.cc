/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "clef.hh"
#include "string.hh"
#include "molecule.hh"
#include "item.hh"

/*
FIXME: should use symbol.

*/
MAKE_SCHEME_CALLBACK(Clef,before_line_breaking);
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
      return SCM_UNSPECIFIED;
    }

  return SCM_UNSPECIFIED;
}

bool
Clef::has_interface (Score_element* me)
{
  return me->has_interface (ly_symbol2scm ("clef-interface"));
}


void
Clef::set_interface (Score_element* me)
{
  me->set_interface (ly_symbol2scm ("clef-interface"));
}


