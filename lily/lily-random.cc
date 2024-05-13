/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2024--2024 Michael Käppler <xmichael-k@web.de>

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

#include "lily-guile.hh"
#include "string-convert.hh"

#include <chrono>
#include <random>
#include <string>

namespace LilyRandom
{
static std::mt19937 randgen {};
constexpr unsigned hex_char_count = 6;
// Calculate the maximum file id that can fit into the specified
// number of chars.
constexpr unsigned max_file_id = 2 << (4 * hex_char_count - 1);

static std::uniform_int_distribution<unsigned> file_id_dist (0, max_file_id);

unsigned
make_rand_seed ()
{
  const auto ticks
    = std::chrono::high_resolution_clock::now ().time_since_epoch ().count ();
  const auto pid = getpid ();
  const auto seed = static_cast<unsigned> (ticks ^ pid);
  return seed;
}

std::string
make_tmpfile_name (const std::string &file_name)
{
  const auto file_id = file_id_dist (randgen);
  const auto tmpfile_name = String_convert::form_string (
    "%s.%0*x", file_name.c_str (), hex_char_count, file_id);
  return tmpfile_name;
}

} // namespace LilyRandom

LY_DEFINE (ly_make_rand_seed, "ly:make-rand-seed", 0, 0, 0, (),
           R"(
Create seed value for initialization of a pseudo-random generator by
combining output from a high-resolution clock with the current
process id.
           )")
{
  return to_scm (LilyRandom::make_rand_seed ());
}

LY_DEFINE (ly_set_rand_seed, "ly:set-rand-seed", 1, 0, 0, (SCM seed),
           R"(
Seed the internal pseudo-random generator with the specified value.
           )")
{
  const auto seed_int = from_scm<unsigned> (seed);
  LilyRandom::randgen.seed (seed_int);
  return SCM_UNSPECIFIED;
}
