/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

class Simultaneous_music_iterator : public Music_iterator
{
public:
  Simultaneous_music_iterator ();
  Simultaneous_music_iterator (Simultaneous_music_iterator const &);
  virtual void derived_substitute (Context *f, Context *t);
  virtual void derived_mark () const;
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_CLASSNAME(Simultaneous_music_iterator);

  /// make a new context for every child.
  bool create_separate_contexts_;

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual bool ok () const;
  virtual bool run_always () const;

protected:
  virtual void process (Moment);

private:
  SCM children_list_;
};

#endif // SIMULTANEOUS_MUSIC_ITERATOR_HH
