/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "volta-repeat-iterator.hh"

#include "music.hh"
#include "repeat-styler.hh"

#include <memory>

bool
Volta_repeat_iterator::empty () const
{
  return !music_get_length () && !music_start_mom ().grace_part_;
}

void
Volta_repeat_iterator::create_children ()
{
  // Do not style repeats inside LyricCombineMusic because the way the
  // Lyric_combine_music_iterator drives the processing tends to place things
  // at the wrong point in time.
  const auto timing_is_accurate
    = !find_above_by_music_type (ly_symbol2scm ("lyric-combine-music"));

  repeat_styler_ = timing_is_accurate
                   ? Repeat_styler::create_volta (this)
                   : Repeat_styler::create_null (this);

  Sequential_iterator::create_children ();
}

void
Volta_repeat_iterator::process (Moment m)
{
  if (!started_)
    {
      // Robustness: Avoid printing a misleading bar line for a zero-duration
      // repeated section.
      if (!empty ())
        {
          // This won't compute the correct lifetime inside \grace.
          const auto &start = get_context ()->now_mom ();
          const auto len = music_get_length () - music_start_mom ();
          repeat_styler_->report_start ({start, start + len});
        }
      started_ = true;
    }

  Sequential_iterator::process (m);

  if (started_ && !stopped_ && (m == music_get_length ()))
    {
      // When there are tail alternatives, Alternative_sequence_iterator
      // issues end-repeat commands.
      if (!empty () && !repeat_styler_->reported_end ())
        {
          repeat_styler_->report_end ();
        }
      stopped_ = true;
    }
}

IMPLEMENT_CTOR_CALLBACK (Volta_repeat_iterator);
