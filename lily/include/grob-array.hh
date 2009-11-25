/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "std-vector.hh"

class Grob_array
{
  vector<Grob*> grobs_;
  bool ordered_;

  DECLARE_SIMPLE_SMOBS (Grob_array);

  Grob_array ();
public:
  bool ordered () const { return ordered_; }
  void set_ordered (bool b) { ordered_ = b; }
  Item *item (vsize i);
  Spanner *spanner (vsize i);
  Grob *grob (vsize i) { return grobs_.at (i); }
  vsize size () const { return grobs_.size (); }
  bool empty () const;
  void remove_duplicates ();
  void clear ();
  void add (Grob *x) { grobs_.push_back (x); }
  void set_array (vector<Grob*> const &src);
  vector<Grob*> &array_reference ();
  vector<Grob*> const &array () const;
  static SCM make_array ();
};

DECLARE_UNSMOB (Grob_array, grob_array);

vector<Grob*> const &ly_scm2link_array (SCM x);
SCM grob_list_to_grob_array (SCM lst);

#endif /* GROB_ARRAY_HH */

