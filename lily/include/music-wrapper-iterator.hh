/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  DECLARE_CLASSNAME(Music_wrapper_iterator);

  virtual void derived_substitute (Context *f, Context *t);
  virtual void derived_mark () const;
  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual bool ok () const;
  virtual bool run_always () const;
protected:
  virtual void process (Moment);

  Music_iterator *child_iter_;
};

#endif /* MUSIC_WRAPPER_ITERATOR_HH */

