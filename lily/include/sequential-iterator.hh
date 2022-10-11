/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
class Sequential_iterator : public Music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  OVERRIDE_CLASS_NAME (Sequential_iterator);

  Sequential_iterator () = default;

  Sequential_iterator (Sequential_iterator const &) = delete;
  Sequential_iterator &operator= (Sequential_iterator const &) = delete;

  void derived_mark () const override;

  Moment pending_moment () const override;
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
  void look_ahead ();
  void pop_element ();

private:
  // all elements subsequent to the element that iter_ is iterating
  SCM remaining_music_ = SCM_EOL;
  // the first element of remaining_music_ that starts at a moment with a main
  // part in the future
  SCM ahead_music_ = SCM_EOL;
  // iterator for the current music element
  Music_iterator *iter_ = nullptr;
  // when iter_ is valid, this is its start moment relative to the zero point
  // of the whole sequence (once iter_ is invalid, see the code)
  Moment iter_start_mom_;
  // the starting point of ahead_music_ within the whole sequence; if
  // ahead_music_ is empty and remaining_music_ is not, this is the ending
  // point of remaining_music_
  Moment ahead_mom_ {Rational::infinity ()};
};

#endif /* SEQUENTIAL_ITERATOR_HH */
