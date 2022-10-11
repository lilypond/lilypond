/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2003--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "gregorian-ligature.hh"

#include "grob.hh"

using std::string;

void
check_prefix (const string &name, int mask, int prefix_set, string *str)
{
  if (prefix_set & mask)
    {
      if (!str->empty ())
        *str += ", ";
      *str += name;
    }
}

string
Gregorian_ligature::prefixes_to_str (Grob *primitive)
{
  string str;
  int prefix_set = from_scm<int> (get_property (primitive, "prefix-set"));
  check_prefix ("virga", VIRGA, prefix_set, &str);
  check_prefix ("stropha", STROPHA, prefix_set, &str);
  check_prefix ("inclinatum", INCLINATUM, prefix_set, &str);
  check_prefix ("auctum", AUCTUM, prefix_set, &str);
  check_prefix ("descendens", DESCENDENS, prefix_set, &str);
  check_prefix ("ascendens", ASCENDENS, prefix_set, &str);
  check_prefix ("oriscus", ORISCUS, prefix_set, &str);
  check_prefix ("quilisma", QUILISMA, prefix_set, &str);
  check_prefix ("deminutum", DEMINUTUM, prefix_set, &str);
  check_prefix ("cavum", CAVUM, prefix_set, &str);
  check_prefix ("linea", LINEA, prefix_set, &str);
  return str;
}

/*
  CHECK ME -- does prefix-set come from here ?

  In a way, yes.  Actually, prefix-set is a property that is written
  by code of GregorianLigatureEngraver that is virtually invoked by a
  subclass like VaticanaLigatureEngraver.  The property is lateron
  read by the associated item class, such as VaticanaLigature.--jr
*/
ADD_INTERFACE (Gregorian_ligature,
               R"(
A gregorian ligature.
               )",

               /* properties */
               R"(
virga
stropha
inclinatum
auctum
descendens
ascendens
oriscus
quilisma
deminutum
cavum
linea
pes-or-flexa
context-info
prefix-set
               )");
