/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "simple-music-iterator.hh"

#include "context.hh"
#include "lily-guile.hh"
#include "moment.hh"
#include "music.hh"

// It might make some sense to derive Fine_iterator from Event_iterator, but
// there are conditions under which Fine_iterator might not send its event, and
// it doesn't seem right to complicate Event_iterator to handle that.
class Fine_iterator final : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());

protected:
  void create_contexts () override;
  void process (Moment) override;
};

void
Fine_iterator::create_contexts ()
{
  descend_to_bottom_context ();
  Simple_music_iterator::create_contexts ();
}

void
Fine_iterator::process (Moment m)
{
  if (!has_started ())
    {
      // Ignore \fine inside LyricCombineMusic because the way the
      // Lyric_combine_music_iterator drives the processing tends to place
      // things at the wrong point in time.
      const auto timing_is_accurate
        = !find_above_by_music_type (ly_symbol2scm ("lyric-combine-music"));

      if (timing_is_accurate)
        {
          auto *m = get_music ()->clone ();
          const auto folded = find_above_by_music_type (
            ly_symbol2scm ("folded-repeated-music"));
          set_property (m, "fine-folded", to_scm (static_cast<bool> (folded)));
          report_event (m);
          m->unprotect ();
        }
    }

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Fine_iterator);
