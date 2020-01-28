/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "audio-element-info.hh"

#include "context.hh"
#include "translator-group.hh"

using std::vector;

Audio_element_info::Audio_element_info (Audio_element *s, Stream_event *r)
{
  elem_ = s;
  origin_trans_ = 0;
  event_ = r;
}

Audio_element_info::Audio_element_info ()
{
  elem_ = 0;
  event_ = 0;
  origin_trans_ = 0;
}

vector<Context *>
Audio_element_info::origin_contexts (Translator *end) const
{
  Context *t = origin_trans_->context ();
  vector<Context *> r;
  do
    {
      r.push_back (t);
      t = t->get_parent_context ();
    }
  while (t && t != end->context ());

  return r;
}
