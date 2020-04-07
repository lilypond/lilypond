/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>
#include <cstdlib>

#include "item.hh"
#include "system.hh"
#include "grob-array.hh"

using std::vector;

// TODO: int is wider than necessary.  Consider changing it to
// System::rank_type.  For now, the decision is not to introduce a new
// instantiation of Interval_t<>.
typedef Interval_t<int> System_range;

static SCM break_criterion;
void
set_break_substitution (SCM criterion)
{
  break_criterion = criterion;
}

/*
  Perform the substitution for a single grob.
*/
Grob *
substitute_grob (Grob *sc)
{
  if (scm_is_integer (break_criterion))
    {
      Item *i = dynamic_cast<Item *> (sc);
      Direction d = to_dir (break_criterion);
      if (i && i->break_status_dir () != d)
        {
          Item *br = i->find_prebroken_piece (d);
          return br;
        }
    }
  else
    {
      System *line
        = unsmob<System> (break_criterion);
      if (sc->get_system () != line)
        sc = sc->find_broken_piece (line);

      /* now: !sc || (sc && sc->get_system () == line) */
      if (!sc)
        return 0;

      /* now: sc && sc->get_system () == line */
      if (!line)
        return sc;

      /*
        We don't return SCM_UNDEFINED for
        suicided grobs, for two reasons

        - it doesn't work (strange disappearing objects)

        - it forces us to mark the parents of a grob, leading to
        a huge recursion in the GC routine.
      */

      if (sc->common_refpoint (line, X_AXIS)
          && sc->common_refpoint (line, Y_AXIS))
        return sc;
      return 0;
    }

  return sc;
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
SCM
do_break_substitution (SCM src)
{
again:

  if (unsmob<Grob> (src))
    {
      Grob *new_ptr = substitute_grob (unsmob<Grob> (src));
      return new_ptr ? new_ptr->self_scm () : SCM_UNDEFINED;
    }
  else if (scm_is_vector (src))
    {
      size_t len = scm_c_vector_length (src);
      SCM nv = scm_c_make_vector (len, SCM_UNDEFINED);
      for (size_t i = 0; i < len; i++)
        {
          scm_c_vector_set_x (nv, i, do_break_substitution (scm_c_vector_ref (src, i)));
        }
    }
  else if (scm_is_pair (src))
    {
      /*
        UGH! breaks on circular lists.
      */
      SCM newcar = do_break_substitution (scm_car (src));
      SCM oldcdr = scm_cdr (src);

      if (SCM_UNBNDP (newcar)
          && (scm_is_pair (oldcdr) || scm_is_null (oldcdr)))
        {
          /*
            This is tail-recursion, ie.

            return do_break_substution (cdr);

            We don't want to rely on the compiler to do this.  Without
            tail-recursion, this easily crashes with a stack overflow.  */
          src = oldcdr;
          goto again;
        }

      return scm_cons (newcar, do_break_substitution (oldcdr));
    }
  else
    return src;

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

System_range
spanner_system_range (Spanner *sp)
{
  System_range rv;

  if (System *st = sp->get_system ())
    rv = System_range (st->get_rank (), st->get_rank ());
  else
    {
      vector<Spanner *> const &bs = sp->broken_intos_;
      if (!bs.empty ())
        {
          rv = System_range (bs.front ()->get_system ()->get_rank (),
                             bs.back ()->get_system ()->get_rank ());
        }
    }
  return rv;
}

System_range
item_system_range (Item *it)
{
  if (System *st = it->get_system ())
    return System_range (st->get_rank (), st->get_rank ());

  System_range sr;
  for (LEFT_and_RIGHT (d))
    {
      Item *bi = it->find_prebroken_piece (d);
      if (bi && bi->get_system ())
        sr.add_point (bi->get_system ()->get_rank ());
    }

  return sr;
}

System_range
grob_system_range (Grob *g)
{
  // ugh: looks like a job for a virtual method
  if (Spanner *s = dynamic_cast<Spanner *> (g))
    return spanner_system_range (s);
  else if (Item *it = dynamic_cast<Item *> (g))
    return item_system_range (it);
  else
    return System_range ();
}

struct Substitution_entry
{
  Grob *grob_;

  /* The all-elements array inside the System is large. To save
     memory, we assume there will not be more than 32k systems, and use
     int16 for the indices, to save some space.
  */
  System::rank_type left_;
  System::rank_type right_;

  void set (Grob *g, System_range sr)
  {
    grob_ = g;
    /*
      duh, don't support scores with more than 32000 systems.
    */
    if (sr.is_empty ())
      {
        /*
          overflow if we don't treat this specially.
        */
        left_ = 1;
        right_ = -1;
      }
    else
      {
        left_ = static_cast<System::rank_type> (sr[LEFT]);
        right_ = static_cast<System::rank_type> (sr[RIGHT]);
      }
  }
  Substitution_entry ()
  {
    grob_ = 0;
    left_ = right_ = -2;
  }

  int length () { return right_ - left_; }
  static int
  item_compare (void const *a, void const *b)
  {
    return ((Substitution_entry *)a)->left_
           - ((Substitution_entry *)b)->left_;
  }

  static int
  spanner_compare (void const *a, void const *b)
  {
    return ((Substitution_entry *)a)->length ()
           - ((Substitution_entry *)b)->length ();
  }
};

bool
Spanner::fast_substitute_grob_array (SCM sym,
                                     Grob_array *grob_array)
{
  int len = grob_array->size ();

  if (grob_array->ordered ())
    return false;

  // TODO: Was this chosen after profiling in 2005?  Maybe it should be
  // revisited.
  if (len < 15)
    return false;

  /*
    We store items on the left, spanners on the right in this vector.

    FIXME: will not multithread.
  */
  static Substitution_entry *vec;
  static int vec_room;

  if (vec_room < len)
    {
      vec = (Substitution_entry *) realloc (vec, sizeof (Substitution_entry) * len);
      vec_room = len;
    }

  System_range system_range = spanner_system_range (this);

  int spanner_index = len;
  int item_index = 0;

  for (vsize i = 0; i < grob_array->size (); i++)
    {
      Grob *g = grob_array->grob (i);

      System_range sr = grob_system_range (g);
      sr.intersect (system_range);

      // ugh: maybe a job for a virtual method
      int idx = 0;
      if (dynamic_cast<Spanner *> (g))
        idx = --spanner_index;
      else if (dynamic_cast<Item *> (g))
        idx = item_index++;

      vec[idx].set (g, sr);
    }

  qsort (vec, item_index,
         sizeof (Substitution_entry), &Substitution_entry::item_compare);

  vector<Slice> item_indices;
  vector<Slice> spanner_indices;
  for (int i = 0; i <= system_range.length (); i++)
    {
      item_indices.push_back (Slice (len, 0));
      spanner_indices.push_back (Slice (len, 0));
    }

  vector<Slice> *arrs[]
  =
  {
    &item_indices, &spanner_indices
  };

  for (int i = 0; i < item_index; i++)
    {
      for (int j = vec[i].left_; j <= vec[i].right_; j++)
        item_indices[j - system_range[LEFT]].add_point (i);
    }

  /*
    sorting vec[spanner_index.. len]
    is a waste of time -- the staff-spanners screw up the
    ordering, since they go across the entire score.
  */
  for (vsize i = spanner_indices.size (); i--;)
    spanner_indices[i] = Slice (spanner_index, len - 1);

  assert (item_index <= spanner_index);

  assert ((broken_intos_.size () == (vsize)system_range.length () + 1)
          || (broken_intos_.empty () && system_range.length () == 0));
  for (vsize i = 0; i < broken_intos_.size (); i++)
    {
      Grob *sc = broken_intos_[i];
      System *l = sc->get_system ();
      set_break_substitution (l ? l->self_scm () : SCM_UNDEFINED);

      SCM newval = sc->internal_get_object (sym);
      if (!unsmob<Grob_array> (newval))
        {
          newval = Grob_array::make_array ();
          set_object (sc, sym, newval);
        }

      Grob_array *new_array = unsmob<Grob_array> (newval);
      for (int k = 0; k < 2; k++)
        for (int j = (*arrs[k])[i][LEFT]; j <= (*arrs[k])[i][RIGHT]; j++)
          {
            Grob *substituted = substitute_grob (vec[j].grob_);
            if (substituted)
              new_array->add (substituted);
          }

#ifdef PARANOIA
      printf ("%d (%d), sp %d (%d)\n",
              item_indices [i].length (), item_index,
              spanner_indices[i].length (), len - spanner_index);

      {
        SCM l1 = substitute_grob_list (grob_list);
        assert (scm_ilength (l1) == scm_ilength (newval));
      }
#endif
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
void
substitute_object_alist (SCM alist, SCM *dest)
{
  SCM old = *dest;
  *dest = SCM_EOL;
  SCM *tail = dest;
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM sym = scm_caar (s);
      SCM val = scm_cdar (s);

      if (Grob_array *orig = unsmob<Grob_array> (val))
        {
          SCM handle = scm_assq (sym, old);
          SCM newval
            = (scm_is_pair (handle))
              ? scm_cdr (handle)
              : Grob_array::make_array ();

          Grob_array *new_arr = unsmob<Grob_array> (newval);
          // TODO: What if new_arr is null?
          new_arr->filter_map_assign (*orig, substitute_grob);
          val = newval;
        }
      else
        val = do_break_substitution (val);

      if (!SCM_UNBNDP (val))
        {
          /*
            for ly:grob? properties, SCM_UNDEFINED could leak out
            through ly:grob-property
          */
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

  for (vsize i = 0; i < broken_intos_.size (); i++)
    {
      Grob *sc = broken_intos_[i];
      System *l = sc->get_system ();
      set_break_substitution (l ? l->self_scm () : SCM_UNDEFINED);

      if (grob_array)
        {
          SCM newval = sc->internal_get_object (sym);
          if (!unsmob<Grob_array> (newval))
            {
              newval = Grob_array::make_array ();
              set_object (sc, sym, newval);
            }
          Grob_array *new_arr = unsmob<Grob_array> (newval);
          new_arr->filter_map_assign (*grob_array, substitute_grob);
        }
      else
        {
          SCM newval = do_break_substitution (val);
          set_object (sc, sym, newval);
        }
    }
}

void
Grob::substitute_object_links (SCM crit, SCM orig)
{
  set_break_substitution (crit);
  substitute_object_alist (orig, &object_alist_);
}
