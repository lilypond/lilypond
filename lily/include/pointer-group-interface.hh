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

#ifndef POINTER_GROUP_INTERFACE_HH
#define POINTER_GROUP_INTERFACE_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

#include <vector>

struct Pointer_group_interface
{
public:
  static vsize count (Grob *, SCM);
  static void add_grob (Grob *, SCM nm, Grob *e);
  static void add_grob (Grob *, SCM nm, SCM x);
  static void add_unordered_grob (Grob *, SCM nm, Grob *);
  static void set_ordered (Grob *, SCM, bool);
  static Grob_array *get_grob_array (Grob *, SCM);
  static Grob *find_grob (Grob *, SCM, bool (*pred) (Grob const *));
};

std::vector<Grob *> const &internal_extract_grob_array (Grob const *elt,
                                                        SCM symbol);
std::vector<Item *> internal_extract_item_array (Grob const *elt, SCM symbol);

#define extract_grob_array(x, prop)                                            \
  internal_extract_grob_array (x, ly_symbol2scm (prop))
#define extract_item_array(x, prop)                                            \
  internal_extract_item_array (x, ly_symbol2scm (prop))

/*
  This is dubious coding style, but lets not risk that we change the
  representation of grob sets again.
*/
#define extract_grob_set(grob, prop, set)                                      \
  std::vector<Grob *> const &set (                                             \
    internal_extract_grob_array (grob, ly_symbol2scm (prop)))
#define extract_item_set(grob, prop, set)                                      \
  std::vector<Item *> set (                                                    \
    internal_extract_item_array (grob, ly_symbol2scm (prop)))

#endif /* POINTER_GROUP_INTERFACE_HH */
