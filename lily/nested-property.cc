/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Copyright (C) 2014--2022 David Kastrup <dak@gnu.org>

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

#include "context.hh"
#include "grob.hh"

// scm_reverse_x without the checks
SCM
fast_reverse_x (SCM lst, SCM tail)
{
  while (!scm_is_null (lst))
    {
      SCM n = scm_cdr (lst);
      scm_set_cdr_x (lst, tail);
      tail = lst;
      lst = n;
    }
  return tail;
}

// copy the spine of lst not including tail, appending newtail
// returns new list.
SCM
partial_list_copy (SCM lst, SCM tail, SCM newtail)
{
  SCM p = SCM_EOL;
  for (; !scm_is_eq (lst, tail); lst = scm_cdr (lst))
    p = scm_cons (scm_car (lst), p);
  return fast_reverse_x (p, newtail);
}

SCM
assq_tail (SCM key, SCM alist, SCM based_on = SCM_EOL)
{
  for (SCM p = alist; !scm_is_eq (p, based_on); p = scm_cdr (p))
    {
      if (scm_is_eq (scm_caar (p), key))
        return p;
    }
  return SCM_BOOL_F;
}

SCM
assv_tail (SCM key, SCM alist, SCM based_on = SCM_EOL)
{
  for (SCM p = alist; !scm_is_eq (p, based_on); p = scm_cdr (p))
    {
      if (scm_is_true (scm_eqv_p (scm_caar (p), key)))
        return p;
    }
  return SCM_BOOL_F;
}

SCM
assoc_tail (SCM key, SCM alist, SCM based_on = SCM_EOL)
{
  if (SCM_IMP (key) || scm_is_symbol (key))
    return assq_tail (key, alist, based_on);
  if (scm_is_number (key) || scm_is_true (scm_char_p (key)))
    return assv_tail (key, alist, based_on);
  for (SCM p = alist; !scm_is_eq (p, based_on); p = scm_cdr (p))
    {
      if (ly_is_equal (scm_caar (p), key))
        return p;
    }
  return SCM_BOOL_F;
}

// Like assq, but removes the found element destructively
SCM
assq_pop_x (SCM key, SCM *alist)
{
  for (SCM p = *alist; scm_is_pair (p); p = *(alist = SCM_CDRLOC (p)))
    {
      if (scm_is_eq (scm_caar (p), key))
        {
          *alist = scm_cdr (p);
          return scm_car (p);
        }
    }
  return SCM_BOOL_F;
}

/*
  Drop key from the list alist..alist_end.
 */
SCM
evict_from_alist (SCM key, SCM alist, SCM alist_end)
{
  SCM p = assoc_tail (key, alist, alist_end);

  if (scm_is_true (p))
    return partial_list_copy (alist, p, scm_cdr (p));
  return alist;
}

// This is the same as
// nested_property_alist (SCM_EOL, prop_path, value) but faster
SCM
nested_create_alist (SCM prop_path, SCM value)
{
  if (scm_is_null (prop_path))
    return value;
  return scm_acons (scm_car (prop_path),
                    nested_create_alist (scm_cdr (prop_path), value), SCM_EOL);
}

/*
  PROP_PATH should be big-to-small ordering
 */

// Take the given alist and replace the given nested property with the
// given value.  Multiple overrides of the same property path are not
// coalesced for efficiency reasons: they are considered rare enough
// to not be worth the cost of detecting them.  When sublists are
// modified, however, we remove the original sublist and copy the
// spine before it.  The cost for finding the sublist has already been
// paid anyway.

// A typical use case for this routine is applying (possibly nested)
// tweaks to a grob property list.

SCM
nested_property_alist (SCM alist, SCM prop_path, SCM value)
{
  // replacement moves to the front.
  SCM key = scm_car (prop_path);
  SCM rest = scm_cdr (prop_path);
  if (scm_is_pair (rest))
    {
      SCM where = assoc_tail (key, alist);

      if (scm_is_false (where))
        return scm_acons (key, nested_create_alist (rest, value), alist);
      return scm_acons (key,
                        nested_property_alist (scm_cdar (where), rest, value),
                        partial_list_copy (alist, where, scm_cdr (where)));
    }
    // Outcommented code would coalesce multiple overrides of the same
    // property
#if 0
  SCM where = assq_tail (alist, key);
  if (scm_is_true (where))
    return scm_acons (key, value,
                      partial_list_copy (alist, where, scm_cdr (where)));
#endif
  return scm_acons (key, value, alist);
}

SCM
nested_property (SCM alist, SCM prop_path, SCM fallback)
{
  for (; scm_is_pair (prop_path); prop_path = scm_cdr (prop_path))
    {
      SCM tail = assoc_tail (scm_car (prop_path), alist);
      if (scm_is_false (tail))
        return fallback;
      alist = scm_cdar (tail);
    }
  return alist;
}

void
set_nested_property (Grob *me, SCM big_to_small, SCM value)
{
  SCM alist = get_property (me, scm_car (big_to_small));

  alist = nested_property_alist (alist, scm_cdr (big_to_small), value);

  set_property (me, scm_car (big_to_small), alist);
}

// This converts an alist with nested overrides in it to a proper
// alist.  The number of nested overrides is known in advance,
// everything up to the last nested override is copied, the tail is
// shared.
//
// The first nalist index has to be a symbol since the conversion
// relies on eq? comparisons, uses some special non-symbol values for
// special purposes, and does validity checking indexed by symbols.
// Subindexing can be done with equal?-comparable indexes, however.

SCM
nalist_to_alist (SCM nalist, int nested)
{
  if (!nested)
    return nalist;
  SCM copied = SCM_EOL;
  SCM partials = SCM_EOL;
  // partials is a alist of partial overrides
  while (nested)
    {
      SCM elt = scm_car (nalist);
      nalist = scm_cdr (nalist);
      SCM key = scm_car (elt);
      if (!scm_is_symbol (key))
        --nested;
      if (scm_is_bool (key))
        {
          if (scm_is_false (key))
            continue;
          elt = scm_cdr (elt);
          key = scm_car (elt);
        }
      if (scm_is_pair (key))
        // nested override: record for key in partial
        {
          SCM pair = scm_sloppy_assq (scm_car (key), partials);
          if (scm_is_false (pair))
            partials = scm_acons (scm_car (key), ly_list (elt), partials);
          else
            scm_set_cdr_x (pair, scm_cons (elt, scm_cdr (pair)));
          continue;
        }
      assert (scm_is_symbol (key));
      // plain override: apply any known corresponding partials
      SCM pair = assq_pop_x (key, &partials);
      if (scm_is_true (pair))
        {
          SCM value = scm_cdr (elt);
          for (SCM pp = scm_cdr (pair); scm_is_pair (pp); pp = scm_cdr (pp))
            value
              = nested_property_alist (value, scm_cdaar (pp), scm_cdar (pp));
          copied = scm_acons (key, value, copied);
        }
      else
        copied = scm_cons (elt, copied);
    }
  // Now need to work off the remaining partials.  All of them are
  // unique, so we can push them to `copied' after resolving without
  // losing information.

  for (; scm_is_pair (partials); partials = scm_cdr (partials))
    {
      SCM pair = scm_car (partials);
      SCM key = scm_car (pair);
      SCM elt = scm_sloppy_assq (key, nalist);
      SCM value = SCM_EOL;
      if (scm_is_true (elt))
        value = scm_cdr (elt);

      for (SCM pp = scm_cdr (pair); scm_is_pair (pp); pp = scm_cdr (pp))
        value = nested_property_alist (value, scm_cdaar (pp), scm_cdar (pp));

      copied = scm_acons (key, value, copied);
    }
  return fast_reverse_x (copied, nalist);
}

#if 0
// Alternative approach: don't unfold those partial overrides while
// they are part of contexts but instead use a special accessor for
// subproperties in the grob.  Not used or tested for now.

SCM
nassq_ref (SCM key, SCM nalist, SCM fallback)
{
  SCM partials = SCM_EOL;
  // partials is list of partial overrides for the given property
  for (SCM p = nalist; scm_is_pair (p); p = scm_cdr (p))
    {
      SCM elt = scm_car (p);
      SCM pkey = scm_car (elt);
      if (scm_is_pair (pkey))
        {
          if (scm_is_eq (scm_car (pkey), key))
            partials = scm_cons (elt, partials);
        }
      else if (scm_is_eq (pkey, key))
        {
          SCM value = scm_cdr (elt);
          for (; scm_is_pair (partials); partials = scm_cdr (partials))
            {
              value = nested_property_alist (value, scm_cdaar (partials),
                                             scm_cdar (partials));
            }
          return value;
        }
    }
  if (scm_is_pair (partials))
    {
      // Bit of a quandary here: we have only subproperty overrides
      // but no main property.  Could be a programming error, but we
      // instead override an empty list.
      SCM value = nested_create_alist (scm_cdaar (partials), scm_cdar (partials));
      partials = scm_cdr (partials);
      for (; scm_is_pair (partials); partials = scm_cdr (partials))
        value = nested_property_alist (value, scm_cdaar (partials),
                                       scm_cdar (partials));
      return value;
    }
  return SCM_UNBNDP (fallback) ? SCM_EOL : fallback;
}

// Also needed for this approach to make sense: an accessor for true
// subproperties.
SCM
nassq_nested_ref (SCM key, SCM subpath, SCM nalist, SCM fallback);
// To be implemented

#endif
