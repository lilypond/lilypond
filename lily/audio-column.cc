/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "audio-column.hh"

#include "audio-item.hh"

Audio_column::Audio_column (Moment when) { when_ = when; }

void
Audio_column::add_audio_item (Audio_item *l)
{
  audio_items_.push_back (l);
  l->audio_column_ = this;
}

Moment
Audio_column::when () const
{
  return when_;
}

int
Audio_column::ticks () const
{
  return int (moment_to_ticks (when_));
}

void
Audio_column::offset_when (Moment m)
{
  when_ += m;
}
