/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2013--2022 Aleksandr Andreev <aleksandr.andreev@gmail.com>

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

#include "kievan-ligature.hh"

#include "international.hh"
#include "item.hh"
#include "warn.hh"

MAKE_SCHEME_CALLBACK (Kievan_ligature, print, "ly:kievan-ligature::print", 1);
SCM
Kievan_ligature::print (SCM)
{
  return SCM_EOL;
}

ADD_INTERFACE (Kievan_ligature,
               R"(
A kievan ligature.
               )",

               /* properties */
               R"(
primitive
padding
               )");
