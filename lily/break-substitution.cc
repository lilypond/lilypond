/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grob.hh"
#include "item.hh"
#include "system.hh"
#include "grob-array.hh"

#include <cassert>

/*
  Grobs contain internal links to other grobs.  For instance,
  every script contains an array of grobs it takes into account
  for positioning.  After a line breaking configuration has been
  picked, spanners and breakable items are broken in as many
  systems as they span.  Break substitution is the process of
  making the systems independent by substituting grobs in the
  internal links with broken grobs living on the same system as
  the grob where the substitution happens.

  There is a little more to it, however.  Breaking breakable items
  is actually done before line breaking (the broken pieces are
  used during pure calculations and line breaking).  Therefore,
  there is a first pass of break substitution before line breaking,
  only applying to the items contained in links held by items.
  Here, the criterion for finding the right broken piece is not
  being on the same system but having the same break direction.
*/

using std::vector;

/*
  Perform the substitution for a single grob.
*/
template <class Criterion>
static Grob *substitute_grob (Criterion, Grob *);

template <>
Grob *
substitute_grob (Direction d, Grob *sc)
{
  if (auto *item = dynamic_cast<Item *> (sc))
    {
      if (item->break_status_dir () != d)
        return item->find_prebroken_piece (d);
    }

  return sc;
}

template <>
Grob *
substitute_grob (System *line, Grob *sc)
{
  assert (sc);
  assert (line);
  // Note and FIXME: sc->get_system () may be nullptr.
  if (sc->get_system () != line)
    sc = sc->find_broken_piece (line);
  // This grob has no broken piece for this system.
  if (!sc)
    return nullptr;
  // TODO: consider returning nullptr if sc is dead
  if (sc->has_in_ancestry (line, X_AXIS) && sc->has_in_ancestry (line, Y_AXIS))
    return sc;
  return nullptr;
}

/*
  Do break substitution in S, using CRITERION. Return new value.
  CRITERION is either a SMOB pointer to the desired line, or a number
  representing the break direction. Do not modify SRC.

  It is rather tightly coded, since it takes a lot of time; it is
  one of the top functions in the profile.

  We don't pass break_criterion as a parameter, since it is
  `constant', but takes up stack space.

  It would be nice if we could do this in-place partially.  We now
  generate a lot of garbage.
*/
template <class Crit>
static SCM
do_break_substitution (Crit break_criterion, SCM src)
{
  if (auto *og = unsmob<Grob> (src))
    {
      auto *g = substitute_grob (break_criterion, og);
      return g ? g->self_scm () : SCM_UNSPECIFIED;
    }
  else if (auto *ga = unsmob<Grob_array> (src))
    {
      Grob_array *new_arr = unsmob<Grob_array> (Grob_array::make_array ());
      // The new array is ordered iff the original is.  That way,
      // when doing the second break substitution (with systems),
      // we'll also use the optimization available for unordered arrays
      // for arrays created by the first substitution (with directions).
      new_arr->set_ordered (ga->ordered ());
      for (Grob *og : ga->array_reference ())
        if (Grob *g = substitute_grob (break_criterion, og))
          new_arr->add (g);
      return new_arr->smobbed_copy ();
    }
  else if (scm_is_vector (src))
    {
      size_t len = scm_c_vector_length (src);
      SCM nv = scm_c_make_vector (len, SCM_UNSPECIFIED);
      for (size_t i = 0; i < len; i++)
        {
          scm_c_vector_set_x (
            nv, i,
            do_break_substitution (break_criterion, scm_c_vector_ref (src, i)));
        }
      return nv;
    }
  else if (scm_is_pair (src))
    {
      SCM dest = SCM_EOL;
      SCM *tail = &dest;
      /* If it's a pair, src could be just any kind of nested data structure.
         However, typical Scheme patterns (lists) have potentially large data in
         the cdr and not the car.  Thus we recurse in the car and keep stack
         depth constant for the cdr (think of it as tail recursion).  This is why
         this loop looks like a list traversal even though src is not
         necessarily a list. */
      do
        {
          SCM new_car = do_break_substitution (break_criterion, scm_car (src));
          *tail = scm_cons (new_car, SCM_EOL);
          tail = SCM_CDRLOC (*tail);
          src = scm_cdr (src);
        }
      while (scm_is_pair (src));
      *tail = do_break_substitution (break_criterion, src);
      return dest;
    }
  return src;
}

/*
  We don't do

  forall b in broken-childs:
  forall p in properties:
  forall g in p (if grob-list):
  g := substitute (g)

  for spanners since this is O (SYSTEMCOUNT * GROBCOUNT), and SYSTEMCOUNT =
  O (GROBCOUNT), we have a quadratic algorithm. --for a single spanner

  This is problematic: with large (long) scores, the costs can be
  significant; especially all-elements in System, can become huge. For
  a typical 50 page score, it requires running through a 100k list 50
  times.

  Instead:

  forall p in properties:
  (if grob list)

  put  grob list in array,

  reorder array so spanners are separate -- O (grobcount)

  find first and last indexes of grobs on a specific system

  for items this is O (itemcount)

  for spanners this is O (sum-of spanner-system-ranges)

  perform the substitution O (sum-of spanner-system-ranges)


  The complexity is harder to determine, but should be subquadratic;

  For the situation above, we run through the entire 100k list once,
  and also (more or less) once through the item part of the 100k (say
  98k elements) of the list.


  These timings were measured without -O2.

  lehre, before 28.98 seconds, after: 27.91 seconds, 3.5 %.

  coriolan, before 2:30, after:  1:59. Increase of 20%.

  moz-k498-p1, before 24.10, after: 19.790s, Increase of 18%
*/

struct Substitution_entry
{
  Grob *grob_;

  vsize left_;
  vsize right_;

  Substitution_entry (Grob *g, const System_rank_interval &sr)
  {
    grob_ = g;
    left_ = sr[LEFT];
    right_ = sr[RIGHT];
  }

  bool operator<(Substitution_entry const &other) const
  {
    return left_ < other.left_;
  }
};

bool
Spanner::fast_substitute_grob_array (SCM sym, Grob_array const *grob_array)
{
  if (grob_array->ordered ())
    return false;

  // TODO: Was this chosen after profiling in 2005?  Maybe it should be
  // revisited.
  if (grob_array->size () < 15)
    return false;

  const auto system_range = spanned_system_rank_interval ();

  std::vector<Substitution_entry> items;
  std::vector<Grob *> spanners;
  for (auto *g : grob_array->array_reference ())
    {
      if (auto *it = dynamic_cast<Item *> (g))
        {
          auto sr = it->spanned_system_rank_interval ();
          sr.intersect (system_range);
          sr -= system_range[LEFT];
          items.emplace_back (g, sr);
        }
      else
        {
          spanners.emplace_back (g);
        }
    }

  std::stable_sort (items.begin (), items.end ());

  vector<Interval_t<vsize>> item_indices (system_range.length () + 1);
  for (vsize i = 0; i < items.size (); i++)
    {
      for (auto j = items[i].left_; j <= items[i].right_; ++j)
        item_indices[j].add_point (i);
    }

  // Sorting spanners is a waste of time: the staff-spanners screw up the
  // ordering because they span the entire score.

  assert (
    (broken_intos_.size () == static_cast<vsize> (system_range.length () + 1))
    || (broken_intos_.empty () && system_range.length () == 0));

  for (vsize i = 0; i < broken_intos_.size (); ++i)
    {
      auto *const sc = broken_intos_[i];
      SCM newval = sc->internal_get_object (sym);
      auto *new_array = unsmob<Grob_array> (newval);
      if (!new_array)
        {
          newval = Grob_array::make_array ();
          set_object (sc, sym, newval);
          new_array = unsmob<Grob_array> (newval);
        }

      auto *const system = sc->get_system ();

      for (auto j = item_indices[i][LEFT]; j <= item_indices[i][RIGHT]; ++j)
        {
          auto *og = items[j].grob_;
          if (auto *g = substitute_grob (system, og))
            new_array->add (g);
        }

      for (auto *og : spanners)
        {
          if (auto *g = substitute_grob (system, og))
            new_array->add (g);
        }
    }

  return true;
}

/*
  Although the substitution can be written as

  property_alist = do_substitution (other_property_alist),

  we have a special function here: we want to invoke a special
  function for lists of grobs. These can be very long for large
  orchestral scores (eg. 1M elements). do_break_substitution () can
  recurse many levels, taking lots of stack space.

  This becomes a problem if lily is linked against guile with
  pthreads. pthreads impose small limits on the stack size.
*/
template <class Crit>
static void
substitute_object_alist (Crit break_criterion, SCM alist, SCM *dest)
{
  *dest = SCM_EOL;
  SCM *tail = dest;
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM sym = scm_caar (s);
      SCM val = scm_cdar (s);

      val = do_break_substitution (break_criterion, val);

      // Don't even set the property if there is no equivalent of
      // the grob satisfying the criterion.  This is legacy, but for
      // now the choice is to not risk breakage.
      if (!scm_is_eq (val, SCM_UNSPECIFIED))
        {
          *tail = scm_cons (scm_cons (sym, val), SCM_EOL);
          tail = SCM_CDRLOC (*tail);
        }
    }
}

void
Spanner::substitute_one_mutable_property (SCM sym, SCM val)
{
  Grob_array *grob_array = unsmob<Grob_array> (val);
  if (grob_array && fast_substitute_grob_array (sym, grob_array))
    return;

  for (auto *sc : broken_intos_)
    {
      auto *system = sc->get_system ();

      SCM newval = do_break_substitution (system, val);
      set_object (sc, sym, newval);
    }
}

void
Grob::substitute_object_links (Direction crit, SCM orig)
{
  substitute_object_alist (crit, orig, &object_alist_);
}

void
Grob::substitute_object_links (System *crit, SCM orig)
{
  substitute_object_alist (crit, orig, &object_alist_);
}
