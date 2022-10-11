/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SCM_HASH_HH
#define SCM_HASH_HH

#include "small-smobs.hh"

/*
  hash table.

  Used for looking up keys by SCM value (keys compared with eq?).
  This is not much more than a C++ wrapper around a standard Guile hashtable.

  TODO: support arbitrary hash and equality procedures in constructor,
  wrapping the Guile 'hashx' interfaces?  Currently, Scheme_hash_table
  is hardwired to use eq? semantics.
*/

class Scheme_hash_table : public Smob0<Scheme_hash_table>
{
  void operator= (Scheme_hash_table const &) = delete;

public:
  int print_smob (SCM, scm_print_state *) const;
  bool try_retrieve (SCM key, SCM *val);
  bool contains (SCM key) const;
  void set (SCM k, SCM v);
  SCM get (SCM k) const;
  void remove (SCM k);
  SCM to_alist () const;
  static SCM make_smob ();
  static bool is_smob (SCM arg) { return scm_is_true (scm_hash_table_p (arg)); }

private:
  SCM hash_tab () const { return self_scm (); }
};

#endif /* SCM_HASH_HH */
