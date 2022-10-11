/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SIMPLE_MUSIC_ITERATOR_HH
#define SIMPLE_MUSIC_ITERATOR_HH

#include "music-iterator.hh"

/*
  Iterator for atomic music objects: events are generated at the
  beginning and at the end of the music.
*/
class Simple_music_iterator : public Music_iterator
{
private:
  Moment pending_moment_;

protected:
  OVERRIDE_CLASS_NAME (Simple_music_iterator);

  bool has_started () const { return pending_moment_ > music_start_mom (); }

public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Simple_music_iterator () = default;

  void create_children () override
  {
    Music_iterator::create_children ();
    pending_moment_ = music_start_mom ();
  }

  void process (Moment m) override
  {
    const auto &length = music_get_length ();
    pending_moment_ = (m < length) ? length : Moment::infinity ();
  }

  Moment pending_moment () const final override { return pending_moment_; }
};

#endif /* SIMPLE_MUSIC_ITERATOR_HH */
