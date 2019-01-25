/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2019 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "grob-info.hh"
#include "grob.hh"
#include "music.hh"
#include "stream-event.hh"
#include "translator-group.hh"

Grob_info::Grob_info (Translator *t, Grob *g)
{
  origin_trans_ = t;
  grob_ = g;

  /*
    assert here, because this is easier to debug.
  */
  assert (g);
}

Grob_info::Grob_info ()
{
  grob_ = 0;
  origin_trans_ = 0;
}

Stream_event *
Grob_info::event_cause () const
{
  SCM cause = grob_->get_property ("cause");
  return unsmob<Stream_event> (cause);
}

Context *
Grob_info::context () const
{
  return origin_trans_->context ();
}

Stream_event *
Grob_info::ultimate_event_cause () const
{
  SCM cause = grob_->self_scm ();
  while (unsmob<Grob> (cause))
    {
      cause = unsmob<Grob> (cause)->get_property ("cause");
    }
  return unsmob<Stream_event> (cause);
}

bool
Grob_info::less (Grob_info i, Grob_info j)
{
  return Grob::less (i.grob (), j.grob ());
}
