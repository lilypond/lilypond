/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "clef.hh"

#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "stencil.hh"

using std::string;

MAKE_SCHEME_CALLBACK (Clef, calc_glyph_name, 1);
SCM
Clef::calc_glyph_name (SCM smob)
{
  Item *s = unsmob<Item> (smob);
  SCM glyph = s->get_property ("glyph");

  if (scm_is_string (glyph))
    {
      string str = ly_scm2string (glyph);

      if (to_boolean (s->get_property ("non-default"))
          && s->break_status_dir () != RIGHT
          && !to_boolean (s->get_property ("full-size-change")))
        {
          str += "_change";
        }

      return ly_string2scm (str);
    }

  s->suicide ();
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Clef, print, 1)
SCM
Clef::print (SCM smob)
{
  Grob *me = unsmob<Grob> (smob);
  SCM glyph_scm = me->get_property ("glyph-name");
  if (!scm_is_string (glyph_scm))
    return SCM_EOL;

  string glyph = string (ly_scm2string (glyph_scm));
  Font_metric *fm = Font_interface::get_default_font (me);
  Stencil out = fm->find_by_name (glyph);
  if (out.is_empty ())
    me->warning (_f ("clef `%s' not found", glyph.c_str ()));

  return out.smobbed_copy ();
}

ADD_INTERFACE (Clef, "A clef sign.",

               /* properties */
               "full-size-change "
               "glyph "
               "glyph-name "
               "non-default ");
