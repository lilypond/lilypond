/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef MUSIC_WRAPPER_ITERATOR_HH
#define MUSIC_WRAPPER_ITERATOR_HH

#include "music-iterator.hh"

/**
   The iterator for a #Music_wrapper#.  Since #Music_wrapper# essentially
   does nothing, this iterator creates a child iterator and delegates
   all work to that child.
*/
class Music_wrapper_iterator : public Music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Music_wrapper_iterator ();
  OVERRIDE_CLASS_NAME (Music_wrapper_iterator);

  Context *get_outlet () const override;
  void set_context (Context *trans) override;
  void derived_substitute (Context *f, Context *t) override;
  void derived_mark () const override;
  void construct_children () override;
  Moment pending_moment () const override;
  void do_quit () override;
  bool ok () const override;
  bool run_always () const override;

protected:
  void process (Moment) override;

  Music_iterator *child_iter_;
};

#endif /* MUSIC_WRAPPER_ITERATOR_HH */
