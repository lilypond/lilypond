/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "calculated-sequential-music.hh"

#include "lily-guile.hh"
#include "music-sequence.hh"
#include "music.hh"
#include "warn.hh"

SCM
Calculated_sequential_music::calc_elements (Music *me)
{
  SCM proc = get_property (me, "elements-callback");
  if (ly_is_procedure (proc))
    return ly_call (proc, to_scm (me));

  programming_error ("calculated sequential music "
                     "cannot find elements-callback");
  return SCM_EOL;
}

MAKE_SCHEME_CALLBACK (Calculated_sequential_music, length,
                      "ly:calculated-sequential-music::length", 1);
SCM
Calculated_sequential_music::length (SCM m)
{
  auto *const me = LY_ASSERT_SMOB (Music, m, 1);
  const auto &result = Music_sequence::cumulative_length (calc_elements (me));
  return result.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Calculated_sequential_music, start,
                      "ly:calculated-sequential-music::start", 1);
SCM
Calculated_sequential_music::start (SCM m)
{
  auto *const me = LY_ASSERT_SMOB (Music, m, 1);
  const auto &result = Music_sequence::first_start (calc_elements (me));
  return result.smobbed_copy ();
}
