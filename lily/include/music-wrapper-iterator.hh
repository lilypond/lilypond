/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

  Context *get_context () const override;
  void set_context (Context *trans) override;
  void derived_mark () const override;
  Moment pending_moment () const override;
  void do_quit () override;
  bool run_always () const override;

  void preorder_walk (const std::function<void (Music_iterator *)> &) override;

protected:
  void create_children () override;
  void create_contexts () override;
  void process (Moment) override;

  Music_iterator *get_child () { return child_iter_; }

private:
  Music_iterator *child_iter_;
};

#endif /* MUSIC_WRAPPER_ITERATOR_HH */
