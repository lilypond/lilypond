/*
  pointer-group-interface.cc -- implement Pointer_group_interface

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "pointer-group-interface.hh"

#include "grob-array.hh"
#include "grob.hh"

int
Pointer_group_interface::count (Grob *me, SCM sym)
{
  Grob_array *arr = unsmob_grob_array (me->internal_get_object (sym));
  return arr ? arr->size () : 0;
}

void
Pointer_group_interface::add_grob (Grob *me, SCM sym, SCM p)
{
  add_grob (me, sym, unsmob_grob (p));
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
  Grob_array *arr = unsmob_grob_array (scm_arr);
  if (!arr)
    {
      scm_arr = Grob_array::make_array ();
      arr = unsmob_grob_array (scm_arr);
      me->set_object (sym, scm_arr);
    }
  return arr;
}

Grob *
Pointer_group_interface::find_grob (Grob *me, SCM sym, bool (*pred) (Grob*))
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

static vector<Grob*> empty_array;

vector<Grob*> const &
ly_scm2link_array (SCM x)
{
  Grob_array *arr = unsmob_grob_array (x);
  return arr ? arr->array () : empty_array;
}

vector<Grob*> const &
internal_extract_grob_array (Grob const *elt, SCM symbol)
{
  return elt
    ? ly_scm2link_array (elt->internal_get_object (symbol))
    : empty_array;
}

vector<Item*>
internal_extract_item_array (Grob const *elt, SCM symbol)
{
  Grob_array *arr = unsmob_grob_array (elt->internal_get_object (symbol));
  vector<Item*> items;
  for (vsize i = 0; arr && i < arr->size (); i++)
    items.push_back (arr->item (i));

  return items;
}
