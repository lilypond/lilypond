/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "item.hh"
#include "music.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "translator-group.hh"

Grob_info::Grob_info (Translator *t, Grob *g)
{
  origin_trans_ = t;
  grob_ = g;
  start_end_ = START;

  /*
    assert here, because this is easier to debug.
  */
  assert (g);
}

Grob_info::Grob_info ()
{
  grob_ = 0;
  start_end_ = START;
  origin_trans_ = 0;
}

Stream_event *
Grob_info::event_cause () const
{
  SCM cause = grob_->get_property ("cause");
  return unsmob_stream_event (cause);
}

vector<Context*>
Grob_info::origin_contexts (Translator *end) const
{
  Context *t = origin_trans_->context ();
  vector<Context*> r;
  do
    {
      r.push_back (t);
      t = t->get_parent_context ();
    }
  while (t && t != end->context ());

  return r;
}

Context *
Grob_info::context () const
{
  return origin_trans_->context ();
}

Spanner *
Grob_info::spanner () const
{
  return dynamic_cast<Spanner *> (grob_);
}

Item *
Grob_info::item () const
{
  return dynamic_cast<Item *> (grob_);
}

Stream_event *
Grob_info::ultimate_event_cause () const
{
  SCM cause = grob_->self_scm ();
  while (unsmob_grob (cause))
    {
      cause = unsmob_grob (cause)->get_property ("cause");
    }
  return unsmob_stream_event (cause);
}
