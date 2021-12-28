/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2022 Juergen Reuter <reuter@ipd.uka.de>

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

#ifndef VATICANA_LIGATURE_HH
#define VATICANA_LIGATURE_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

struct Vaticana_ligature
{
  DECLARE_SCHEME_CALLBACK (brew_ligature_primitive, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

/*
 * Ligature context info: these attributes are derived from the head
 * prefixes by considering the current and the two neighbouring heads.
 *
 * The below definition extends those in gregorian-ligature.hh.
 */
#define STACKED_HEAD 0x0100 // this head is stacked on the previous one

#endif // VATICANA_LIGATURE_HH
