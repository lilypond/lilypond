/*
  clef.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "clef.hh"

#include "string.hh"
#include "stencil.hh"
#include "item.hh"
#include "font-interface.hh"

/*
 FIXME: should use symbol for #'style.
*/
MAKE_SCHEME_CALLBACK (Clef, before_line_breaking, 1);
SCM
Clef::before_line_breaking (SCM smob)
{
  Item *s = unsmob_item (smob);
  SCM glyph = s->get_property ("glyph-name");

  if (!scm_is_string (glyph))
    s->suicide ();
  else
    {
      String str = ly_scm2string (glyph);

      if (to_boolean (s->get_property ("non-default"))
	  && s->break_status_dir () != RIGHT
	  && !to_boolean (s->get_property ("full-size-change")))
	{
	  str += "_change";
	  s->set_property ("glyph-name", scm_makfrom0str (str.to_str0 ()));	
	}
    }

  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Clef, print, 1)
SCM
Clef::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  SCM glyph_scm = me->get_property ("glyph-name");
  if (!scm_is_string (glyph_scm))
    return SCM_EOL;

  String glyph = String (ly_scm2string (glyph_scm));
  Font_metric *fm = Font_interface::get_default_font (me);
  Stencil out = fm->find_by_name (glyph);
  if (out.is_empty ())
    me->warning (_f ("clef `%s' not found", glyph.to_str0 ()));
  return out.smobbed_copy ();
}

ADD_INTERFACE (Clef, "clef-interface",
  "A clef sign",
  "non-default full-size-change glyph-name");

