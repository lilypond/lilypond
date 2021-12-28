/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grace-iterator.hh"
#include "global-context.hh"
#include "music.hh"

void
Grace_iterator::process (Moment m)
{
  Moment main (-music_start_mom ().grace_part_ + m.grace_part_);

  // GraceChange is announced in order to make the Grace_engraver able
  // to distinguish \stemNeutral \grace { ... and \grace { \stemNeutral ...
  const auto now_in_grace = static_cast<bool> (m.grace_part_);
  if (in_grace_ != now_in_grace)
    {
      auto *child = get_child ();
      if (child && child->get_context ())
        {
          send_stream_event (child->get_context (), "GraceChange", origin ());
        }
    }
  in_grace_ = now_in_grace;

  Music_wrapper_iterator::process (main);

  /* We can safely do this, since \grace should always be inside
     sequential.  */
  descend_to_child (get_child ()->get_context ());
}

Moment
Grace_iterator::pending_moment () const
{
  auto cp = Music_wrapper_iterator::pending_moment ();
  if (!isinf (cp.main_part_))
    {
      cp = {0, music_start_mom ().grace_part_ + cp.main_part_};
    }
  return cp;
}

IMPLEMENT_CTOR_CALLBACK (Grace_iterator);
