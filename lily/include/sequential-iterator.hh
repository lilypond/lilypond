/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SEQUENTIAL_ITERATOR_HH
#define SEQUENTIAL_ITERATOR_HH

#include "music-iterator.hh"

/** Sequential_music iteration: walk each element in turn, and
    construct an iterator for every element.
*/

// Garbage collection timing technicality:

struct Preinit_Sequential_iterator
{
  // iterator for the current music element
  Music_iterator *iter_ = nullptr;
  // Music that starts after iter_'s own music.
  SCM uniterated_music_ = SCM_UNDEFINED;
};


class Sequential_iterator : Preinit_Sequential_iterator, public Music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  OVERRIDE_CLASS_NAME (Sequential_iterator);

  Sequential_iterator () = default;

  Sequential_iterator (Sequential_iterator const &) = delete;
  Sequential_iterator &operator= (Sequential_iterator const &) = delete;

  void derived_mark () const override;

  Moment pending_moment () const final override;
  Moment parent_time (Moment) const override;
  void do_quit () override;

  void preorder_walk (const std::function<void (Music_iterator *)> &) override;

protected:
  void create_children () override;
  void create_contexts () override;
  void process (Moment) override;
  bool run_always () const override;

protected:
  const Music_iterator *get_child () const { return iter_; }
  virtual void next_element () {}

private:
  void get_iter ();
  void pop_element ();

  // convert the Sequential_iterator's timing to iter_'s timing
  Moment iter_moment (Moment mom) const
  {
    if (mom.main_part_ - iter_offset_.main_part_
        == iter_->music_get_length ().main_part_)
      return mom - iter_offset_;
    return mom - iter_offset_.main_part_;
  }

private:
  // music that has not yet been looked at for accounting purposes.  This is
  // after the first element of uniterated_music_ that starts at a moment with
  // a main part in the future
  SCM unscanned_music_ = SCM_UNDEFINED;
  // For a detailed explanation of the following time-keeping variable, see the
  // definition of Sequential_iterator::get_iter
  //
  Moment iter_offset_ = {0, 0}; // offset of iter_'s timing
};


#endif /* SEQUENTIAL_ITERATOR_HH */
