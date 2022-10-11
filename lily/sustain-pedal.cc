/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/
#include "grob.hh"
#include "stencil.hh"
#include "font-interface.hh"

using std::string;

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

MAKE_SCHEME_CALLBACK (Sustain_pedal, print, "ly:sustain-pedal::print", 1);
SCM
Sustain_pedal::print (SCM smob)
{
  auto *const e = LY_ASSERT_SMOB (Grob, smob, 1);

  Stencil mol;
  SCM glyph = get_property (e, "text");
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
