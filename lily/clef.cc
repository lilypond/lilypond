/*
  clef.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "clef.hh"
#include "string.hh"
#include "molecule.hh"
#include "item.hh"
#include "font-interface.hh"

/*
 FIXME: should use symbol for #'style.
*/
MAKE_SCHEME_CALLBACK (Clef,before_line_breaking,1);
SCM
Clef::before_line_breaking (SCM smob)
{
  Item *s = unsmob_item (smob);

  SCM glyph = s->get_grob_property ("glyph-name");
  
  if (gh_string_p (glyph))
    {
      String str = ly_scm2string (glyph);

      if (to_boolean (s->get_grob_property ("non-default"))
	  && s->break_status_dir () != RIGHT
	  && !to_boolean (s->get_grob_property ("full-size-change")))
	{
	  str += "_change";
	  s->set_grob_property ("glyph-name", scm_makfrom0str (str.to_str0 ()));	  
	}
    }
  else
    {
      s->suicide ();
      return SCM_UNSPECIFIED;
    }

  return SCM_UNSPECIFIED;
}




MAKE_SCHEME_CALLBACK (Clef,brew_molecule,1)
SCM
Clef::brew_molecule (SCM smob) 
{
  Grob *me = unsmob_grob (smob);
  SCM glyph_scm = me->get_grob_property ("glyph-name");
  if (!gh_string_p (glyph_scm))
    return SCM_EOL;

  String glyph = String (ly_scm2string (glyph_scm));
  Font_metric *fm = Font_interface::get_default_font (me);
  Molecule out = fm->find_by_name (glyph);
  if (out.empty_b())
    {
      me->warning (_f ("clef `%s' not found", glyph.to_str0 ()));
    }
  return out.smobbed_copy ();
}


ADD_INTERFACE (Clef, "clef-interface",
  "A clef sign",
  "non-default full-size-change glyph-name");

