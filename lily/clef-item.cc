/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "clef.hh"
#include "string.hh"
#include "molecule.hh"
#include "item.hh"
#include "font-interface.hh"

/*
 FIXME: should use symbol for #'style.
*/
MAKE_SCHEME_CALLBACK(Clef,before_line_breaking,1);
SCM
Clef::before_line_breaking (SCM smob)
{
  Item * s = dynamic_cast<Item*> (unsmob_grob (smob));

  SCM glyph = s->get_grob_property ("glyph");
  
  if (gh_string_p (glyph))
    {
      String str = ly_scm2string (glyph);

      if (to_boolean (s->get_grob_property ("non-default")) &&
	  s->break_status_dir() != RIGHT &&
	  to_boolean (s->get_grob_property ("full-size-change")))
	{
	  str += "_change";
	  s->set_grob_property ("glyph", ly_str02scm (str.ch_C()));	  
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
Clef::has_interface (Grob* me)
{
  return me->has_interface (ly_symbol2scm ("clef-interface"));
}


void
Clef::set_interface (Grob* me)
{
  me->set_interface (ly_symbol2scm ("clef-interface"));
}

MAKE_SCHEME_CALLBACK(Clef,brew_molecule,1)
SCM
Clef::brew_molecule (SCM smob) 
{
  Grob * sc = unsmob_grob (smob);
  SCM glyph = sc->get_grob_property ("glyph");
  if (gh_string_p (glyph))
    {
      return Font_interface::get_default_font (sc)->find_by_name (String (ly_scm2string (glyph))).smobbed_copy ();
    }
  else
    {
      return SCM_EOL;
    }
}
