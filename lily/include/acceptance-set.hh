/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#ifndef ACCEPTANCE_SET_HH
#define ACCEPTANCE_SET_HH

#include "lily-guile.hh"

// This is an ordered list of scheme values identifying things that are
// "acceptable."  It optionally includes a default, which is kept at the front
// of the list.
struct Acceptance_set
{
private:
  SCM accepted_;
  SCM default_;

public:
  Acceptance_set ()
    : accepted_ (SCM_EOL),
      default_ (SCM_EOL)
  {
  }

  Acceptance_set (const Acceptance_set &) = delete;
  Acceptance_set &operator= (const Acceptance_set &) = delete;

  Acceptance_set &assign_copy (const Acceptance_set &other)
  {
    accepted_ = scm_list_copy (other.accepted_);
    default_ = scm_is_pair (accepted_) ? scm_car (accepted_) : SCM_EOL;
    return *this;
  }

  void gc_mark () const
  {
    scm_gc_mark (accepted_);
    // if default_ references an item, it is held in accepted_
  }

  // get the full list of acceptable items; if there is a default, it is first
  SCM get_list () const { return accepted_; }

  // get the default item (SCM_EOL if there isn't one)
  SCM get_default () const { return default_; }

  bool has_default () const { return !scm_is_null (default_); }

  // put the given item at the front of the list, but not in front of the
  // default
  void accept (SCM item);

  // accept the given item and set it as the default
  void accept_default (SCM item);

  // remove the given item from the set
  void deny (SCM item);
};

#endif /* ACCEPTANCE_SET_HH */
