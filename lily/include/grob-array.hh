/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef GROB_ARRAY_HH
#define GROB_ARRAY_HH

#include "lily-proto.hh"
#include "smobs.hh"

#include <vector>

class Grob_array : public Simple_smob<Grob_array>, private std::vector<Grob *>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;

private:
  bool ordered_;

  Grob_array ();

public:
  bool ordered () const { return ordered_; }
  void set_ordered (bool b) { ordered_ = b; }
  void remove_duplicates ();
  void set_array (std::vector<Grob *> const &src) { vector::operator= (src); }
  std::vector<Grob *> const &array () const
  {
    return static_cast<const std::vector<Grob *> &> (*this);
  }
  static SCM make_array ();

public: // exposed subset of vector interface
  using vector::value_type;

  // We're not using vector::at because LilyPond is built without support for
  // exceptions, so at() with invalid bounds would simply terminate -- better
  // than undefined behavior, but not serving the user properly.  Just use
  // operator[] and check your own indexes.
  using vector::back;
  using vector::begin;
  using vector::cbegin;
  using vector::cend;
  using vector::clear;
  using vector::empty;
  using vector::end;
  using vector::front;
  using vector::insert;
  using vector::operator[];
  using vector::pop_back;
  using vector::push_back;
  using vector::size;
};

std::vector<Grob *> const &ly_scm2link_array (SCM x);
SCM grob_list_to_grob_array (SCM lst);
SCM grob_array_to_list (Grob_array *array);

#endif /* GROB_ARRAY_HH */
