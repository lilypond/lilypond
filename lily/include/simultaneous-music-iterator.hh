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

#ifndef SIMULTANEOUS_MUSIC_ITERATOR_HH
#define SIMULTANEOUS_MUSIC_ITERATOR_HH

#include "music-iterator.hh"

class Simultaneous_music_iterator final : public Music_iterator
{
public:
  Simultaneous_music_iterator ();
  Simultaneous_music_iterator (Simultaneous_music_iterator const &);
  void derived_substitute (Context *f, Context *t) override;
  void derived_mark () const override;
  DECLARE_SCHEME_CALLBACK (constructor, ());
  OVERRIDE_CLASS_NAME (Simultaneous_music_iterator);

  /// make a new context for every child.
  bool create_separate_contexts_;

  void construct_children () override;
  Moment pending_moment () const override;
  void do_quit () override;
  bool ok () const override;
  bool run_always () const override;

protected:
  void process (Moment) override;

private:
  SCM children_list_;
};

#endif // SIMULTANEOUS_MUSIC_ITERATOR_HH
