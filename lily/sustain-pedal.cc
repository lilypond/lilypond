/*
  sustain-pedal.cc -- implement Sustain_pedal

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#include "grob.hh"
#include "stencil.hh"
#include "font-interface.hh"

// update comment --hwn 
/*
  Urg.
  This is almost text
  Problem is:
  * we have no kerning
  * symbols are at wrong place in font



  Properties:

  glyph -- text string (TODO: make one large glyph of the Ped symbol, removes need for member_print ())
*/

/*
  FIXME. Need to use markup.
*/
struct Sustain_pedal
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

MAKE_SCHEME_CALLBACK (Sustain_pedal, print, 1);
SCM
Sustain_pedal::print (SCM smob)
{
  Grob *e = unsmob_grob (smob);

  Stencil mol;
  SCM glyph = e->get_property ("text");
  if (!scm_is_string (glyph))
    return mol.smobbed_copy ();

  string text = ly_scm2string (glyph);

  for (ssize i = 0; i < text.length (); i++)
    {
      string idx ("pedal.");
      if (text.substr (i, 3) == "Ped")
	{
	  idx += "Ped";
	  i += 2;
	}
      else
	idx += string (&text.c_str ()[i], 1);
      Stencil m = Font_interface::get_default_font (e)->find_by_name (idx);
      if (!m.is_empty ())
	mol.add_at_edge (X_AXIS, RIGHT, m, 0);
    }

  return mol.smobbed_copy ();
}

