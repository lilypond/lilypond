/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

class Grob_array : public Simple_smob<Grob_array>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;

private:
  std::vector<Grob *> grobs_;
  bool ordered_;

  Grob_array ();

public:
  bool ordered () const { return ordered_; }
  void set_ordered (bool b) { ordered_ = b; }
  Grob *grob (vsize i) const { return grobs_.at (i); }
  vsize size () const { return grobs_.size (); }
  bool empty () const { return grobs_.empty (); }
  void remove_duplicates ();
  void clear () { grobs_.clear (); }
  void add (Grob *x) { grobs_.push_back (x); }
  void set_array (std::vector<Grob *> const &src) { grobs_ = src; }
  std::vector<Grob *> &array_reference () { return grobs_; }
  const std::vector<Grob *> &array_reference () const { return grobs_; }
  std::vector<Grob *> const &array () const { return grobs_; }
  static SCM make_array ();

  // Remove grobs that do not satisfy the predicate, leaving the order
  // unchanged.
  void filter (bool (*predicate) (const Grob *));
};

std::vector<Grob *> const &ly_scm2link_array (SCM x);
SCM grob_list_to_grob_array (SCM lst);
SCM grob_array_to_list (Grob_array *array);

#endif /* GROB_ARRAY_HH */
