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
  Grob *grob (vsize i) const { return at (i); }
  void remove_duplicates ();
  void add (Grob *x) { push_back (x); }
  void set_array (std::vector<Grob *> const &src) { vector::operator= (src); }
  std::vector<Grob *> &array_reference ()
  {
    return static_cast<std::vector<Grob *> &> (*this);
  }
  const std::vector<Grob *> &array_reference () const
  {
    return static_cast<const std::vector<Grob *> &> (*this);
  }
  std::vector<Grob *> const &array () const
  {
    return static_cast<const std::vector<Grob *> &> (*this);
  }
  static SCM make_array ();

public: // exposed subset of vector interface
  using vector::clear;
  using vector::empty;
  using vector::size;
};

std::vector<Grob *> const &ly_scm2link_array (SCM x);
SCM grob_list_to_grob_array (SCM lst);
SCM grob_array_to_list (Grob_array *array);

#endif /* GROB_ARRAY_HH */
