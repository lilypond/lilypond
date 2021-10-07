/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "event-iterator.hh"

#include "context.hh"
#include "global-context.hh"
#include "lily-guile.hh"
#include "moment.hh"
#include "music.hh"

class Fine_iterator final : public Event_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  void process (Moment) override;
};

void
Fine_iterator::process (Moment m)
{
  if (!has_started ())
    {
      // Outside a folded repeat, stop iterating.
      if (!find_above_by_music_type (ly_symbol2scm ("folded-repeated-music")))
        find_global_context (get_context ())->set_final_moment ();
    }

  // In any case, report the Fine event.
  Event_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Fine_iterator);
