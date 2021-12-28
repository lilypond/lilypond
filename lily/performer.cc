/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "context.hh"
#include "performer.hh"
#include "performer-group.hh"
#include "warn.hh"

Performer_group *
Performer::get_group () const
{
  // safe: Performers belong to Performer_groups
  return static_cast<Performer_group *> (Translator::get_group ());
}

void
Performer::acknowledge_audio_element (Audio_element_info)
{
}

void
Performer::announce_element (Audio_element_info i)
{
  if (!i.origin_trans_)
    i.origin_trans_ = this;

  get_group ()->announce_element (i);
}
