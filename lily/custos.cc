/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Juergen Reuter <reuter@ipd.uka.de>

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

/* TODO:

- do not show if a clef change immediately follows in the next line

- decide: do or do not print custos if the next line starts with a rest
*/

#include "custos.hh"
#include "direction.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "note-head.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"

#include <cmath> // rint
#include <cstdio>

using std::string;

MAKE_SCHEME_CALLBACK (Custos, print, "ly:custos::print", 1);
SCM
Custos::print (SCM smob)
{
  auto *const me = unsmob<Item> (smob);

  SCM scm_style = get_property (me, "style");
  string style;
  if (scm_is_symbol (scm_style))
    style = ly_symbol2string (scm_style);
  else
    style = "mensural";

  /*
   * Shall we use a common custos font character regardless if on
   * staffline or not, or shall we use individual font characters
   * for both cases?
   */
  bool adjust = true;

  int neutral_pos = from_scm (get_property (me, "neutral-position"), 0);
  Direction neutral_direction
    = from_scm<Direction> (get_property (me, "neutral-direction"));

  int pos = Staff_symbol_referencer::get_rounded_position (me);

  string font_char = "custodes." + style + ".";
  if (pos < neutral_pos)
    font_char += "u";
  else if (pos > neutral_pos)
    font_char += "d";
  else if (neutral_direction == UP)
    font_char += "u";
  else if (neutral_direction == DOWN)
    font_char += "d";
  else // auto direction; not yet supported -> use "d"
    font_char += "d";

  if (adjust)
    font_char += Staff_symbol_referencer::on_line (me, pos) ? "1" : "0";
  else
    font_char += "2";

  Stencil stencil
    = Font_interface::get_default_font (me)->find_by_name (font_char);
  if (stencil.is_empty ())
    {
      me->warning (_f ("custos `%s' not found", font_char));
      return SCM_EOL;
    }

  return stencil.smobbed_copy ();
}

ADD_INTERFACE (Custos,
               R"(
A custos object.  @code{style} can have four valid values: @code{mensural},
@code{vaticana}, @code{medicaea}, and @code{hufnagel}.  @code{mensural} is the
default style.
               )",

               /* properties */
               R"(
style
neutral-position
neutral-direction
               )");
