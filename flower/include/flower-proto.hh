/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys

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

#ifndef FLOWER_PROTO_HH
#define FLOWER_PROTO_HH

#include "real.hh"

#include <cstdint>
#include <cstddef>

class Offset;
class Rational;

using Byte = uint8_t; // C++17 has std::byte
using I64 = int64_t;
using U64 = uint64_t;

typedef size_t vsize;
static constexpr vsize VPOS (-1);

#endif /* FLOWER_PROTO_HH */
