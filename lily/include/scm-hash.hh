/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "smobs.hh"

/*
  hash table.

  1. ALWAYS USE THIS AS VIA A POINTER, i.e.

  class Foo {
  Scheme_hash_table * tab;
  };

  and NOT

  class Foo {
  Scheme_hash_table tab;
  }


  2. UPON DESTRUCTION, DO

  scm_gc_unprotect_object (tab->self_scm_);
*/

class Scheme_hash_table
{
public:
  bool try_retrieve (SCM key, SCM *val);
  bool contains (SCM key) const;
  void set (SCM k, SCM v);
  SCM get (SCM k) const;
  void remove (SCM k);
  Scheme_hash_table ();
  void operator = (Scheme_hash_table const &);
  Scheme_hash_table (Scheme_hash_table const &);
  SCM to_alist () const;

private:
  SCM hash_tab_;
  void copy (Scheme_hash_table const &src);
  DECLARE_SMOBS (Scheme_hash_table);
};

#endif /* SCM_HASH_HH */

