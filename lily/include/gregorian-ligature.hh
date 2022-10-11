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

#ifndef GREGORIAN_LIGATURE_HH
#define GREGORIAN_LIGATURE_HH

class Grob;
#include "std-string.hh"
#include "grob-interface.hh"

class Gregorian_ligature
{
public:
  static std::string prefixes_to_str (Grob *);
};

/*
 * Head prefixes: these bit-mask constants are used to represent
 * attributes immediately derived from user input (e.g. by the user
 * setting a gregorian ligature grob property or using the "\~"
 * keyword).  If the according bit of the head prefix value is set,
 * the attribute applies for this head.  The binary opereator "\~" is
 * treated like a prefix for the head that follows the operator, but
 * does not affect the head that precedes the operator, if any.
 */
#define VIRGA 0x0001        // attribute "\virga"
#define STROPHA 0x0002      // attribute "\stropha"
#define INCLINATUM 0x0004   // attribute "\inclinatum"
#define AUCTUM 0x0008       // attribute "\auctum"
#define DESCENDENS 0x0010   // attribute "\descendens"
#define ASCENDENS 0x0020    // attribute "\ascendens"
#define ORISCUS 0x0040      // attribute "\oriscus"
#define QUILISMA 0x0080     // attribute "\quilisma"
#define DEMINUTUM 0x0100    // attribute "\deminutum"
#define CAVUM 0x0200        // attribute "\cavum"
#define LINEA 0x0400        // attribute "\linea"
#define PES_OR_FLEXA 0x0800 // operator "\~"

/*
 * Ligature context info: these attributes are derived from the head
 * prefixes by considering the current and the two neighbouring heads.
 *
 * These definitions may be extended by more specific Gregorian
 * ligatures such as vaticana-ligature.hh.
 */
#define PES_LOWER 0x0001   // this is a head before "\~" in an ascending melody
#define PES_UPPER 0x0002   // this is a head after "\~" in an ascending melody
#define FLEXA_LEFT 0x0004  // this is a head before "\~" in a descending melody
#define FLEXA_RIGHT 0x0008 // this is a head after "\~" in a descending melody
#define AFTER_DEMINUTUM 0x0020 // previous head was a deminutum

#endif /* GREGORIAN_LIGATURE_HH */
