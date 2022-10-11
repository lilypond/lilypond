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

#include "pointer-group-interface.hh"

#include "grob-array.hh"
#include "grob.hh"
#include "item.hh"

using std::vector;

vsize
Pointer_group_interface::count (Grob *me, SCM sym)
{
  Grob_array *arr = unsmob<Grob_array> (me->internal_get_object (sym));
  return arr ? arr->size () : 0;
}

void
Pointer_group_interface::add_grob (Grob *me, SCM sym, SCM p)
{
  add_grob (me, sym, unsmob<Grob> (p));
}

void
Pointer_group_interface::set_ordered (Grob *me, SCM sym, bool ordered)
{
  Grob_array *arr = get_grob_array (me, sym);
  arr->set_ordered (ordered);
}

Grob_array *
Pointer_group_interface::get_grob_array (Grob *me, SCM sym)
{
  SCM scm_arr = me->internal_get_object (sym);
  Grob_array *arr = unsmob<Grob_array> (scm_arr);
  if (!arr)
    {
      scm_arr = Grob_array::make_array ();
      arr = unsmob<Grob_array> (scm_arr);
      set_object (me, sym, scm_arr);
    }
  return arr;
}

Grob *
Pointer_group_interface::find_grob (Grob *me, SCM sym,
                                    bool (*pred) (Grob const *))
{
  Grob_array *arr = get_grob_array (me, sym);

  for (vsize i = 0; i < arr->size (); i++)
    if (pred (arr->grob (i)))
      return arr->grob (i);

  return 0;
}

void
Pointer_group_interface::add_grob (Grob *me, SCM sym, Grob *p)
{
  Grob_array *arr = get_grob_array (me, sym);
  arr->add (p);
}

void
Pointer_group_interface::add_unordered_grob (Grob *me, SCM sym, Grob *p)
{
  Grob_array *arr = get_grob_array (me, sym);
  arr->add (p);
  arr->set_ordered (false);
}

static vector<Grob *> empty_array;

vector<Grob *> const &
ly_scm2link_array (SCM x)
{
  Grob_array *arr = unsmob<Grob_array> (x);
  return arr ? arr->array () : empty_array;
}

vector<Grob *> const &
internal_extract_grob_array (Grob const *elt, SCM symbol)
{
  return elt ? ly_scm2link_array (elt->internal_get_object (symbol))
             : empty_array;
}

vector<Item *>
internal_extract_item_array (Grob const *elt, SCM symbol)
{
  Grob_array *arr = unsmob<Grob_array> (elt->internal_get_object (symbol));
  vector<Item *> items;
  for (vsize i = 0; arr && i < arr->size (); i++)
    items.push_back (dynamic_cast<Item *> (arr->grob (i)));

  return items;
}
