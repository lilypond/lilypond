/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
#include "performer-group.hh"
#include "warn.hh"


Performer_group *
Performer::get_daddy_performer () const
{
  return
    dynamic_cast<Performer_group *> (get_daddy_translator ());
}

void
Performer::acknowledge_audio_element (Audio_element_info)
{
}

void
Performer::create_audio_elements ()
{
}

void
Performer::announce_element (Audio_element_info i)
{
  if (!i.origin_trans_)
    i.origin_trans_ = this;

  get_daddy_performer ()->announce_element (i);
}
