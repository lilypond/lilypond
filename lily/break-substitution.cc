/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
using namespace std;

#include "item.hh"
#include "system.hh"
#include "grob-array.hh"

static SCM break_criterion;
void
set_break_subsititution (SCM criterion)
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
	= dynamic_cast<System *> (unsmob_grob (break_criterion));
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

  if (unsmob_grob (src))
    {
      Grob *new_ptr = substitute_grob (unsmob_grob (src));
      return new_ptr ? new_ptr->self_scm () : SCM_UNDEFINED;
    }
  else if (scm_is_vector (src))
    {
      int len = scm_c_vector_length (src);
      SCM nv = scm_c_make_vector (len, SCM_UNDEFINED);
      for (int i = 0; i < len; i++)
	{
	  SCM si = scm_from_int (i);
	  scm_vector_set_x (nv, si,
			    do_break_substitution (scm_vector_ref (src, si)));
	}
    }
  else if (scm_is_pair (src))
    {
      /*
	UGH! breaks on circular lists.
      */
      SCM newcar = do_break_substitution (scm_car (src));
      SCM oldcdr = scm_cdr (src);

      if (newcar == SCM_UNDEFINED
	  && (scm_is_pair (oldcdr) || oldcdr == SCM_EOL))
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
  Perform substitution on GROB_LIST using a constant amount of stack.
*/
vector<Grob*> temporary_substition_array;
void
substitute_grob_array (Grob_array *grob_arr, Grob_array *new_arr)
{
  vector<Grob*> &old_grobs (grob_arr->array_reference ());
  vector<Grob*> *new_grobs (new_arr == grob_arr
			       ? & temporary_substition_array
			       : &new_arr->array_reference ());

  new_grobs->resize (old_grobs.size ());
  Grob **array = (Grob **) new_grobs->data ();
  Grob **ptr = array;
  for (vsize i = 0; i < old_grobs.size (); i++)
    {
      Grob *orig = old_grobs[i];
      Grob *new_grob = substitute_grob (orig);
      if (new_grob)
	*ptr++ = new_grob;
    }

  new_grobs->resize (ptr - array);
  if (new_arr == grob_arr)
    new_arr->set_array (*new_grobs);
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

Slice
spanner_system_range (Spanner *sp)
{
  Slice rv;

  if (System *st = sp->get_system ())
    rv = Slice (st->get_rank (), st->get_rank ());
  else
    {
      if (sp->broken_intos_.size ())
	rv = Slice (sp->broken_intos_[0]->get_system ()->get_rank (),
		    sp->broken_intos_.back ()->get_system ()->get_rank ());
    }
  return rv;
}

Slice
item_system_range (Item *it)
{
  if (System *st = it->get_system ())
    return Slice (st->get_rank (), st->get_rank ());

  Slice sr;
  Direction d = LEFT;
  do
    {
      Item *bi = it->find_prebroken_piece (d);
      if (bi && bi->get_system ())
	sr.add_point (bi->get_system ()->get_rank ());
    }
  while (flip (&d) != LEFT);

  return sr;
}

Slice
grob_system_range (Grob *g)
{
  if (Spanner *s = dynamic_cast<Spanner *> (g))
    return spanner_system_range (s);
  else if (Item *it = dynamic_cast<Item *> (g))
    return item_system_range (it);
  else
    return Slice ();
}

struct Substitution_entry
{
  Grob *grob_;

  /* Assumption: we have less than 32k paper columns. */
  short left_;
  short right_;

  void set (Grob *g, Slice sr)
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
	left_ = sr[LEFT];
	right_ = sr[RIGHT];
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

  Slice system_range = spanner_system_range (this);

  int spanner_index = len;
  int item_index = 0;

  for (vsize i = 0; i < grob_array->size (); i++)
    {
      Grob *g = grob_array->grob (i);

      Slice sr = grob_system_range (g);
      sr.intersect (system_range);

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
    = {
    &item_indices, &spanner_indices
  };

  for (int i = 0; i < item_index;i++)
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
      set_break_subsititution (l ? l->self_scm () : SCM_UNDEFINED);

      SCM newval = sc->internal_get_object (sym);
      if (!unsmob_grob_array (newval))
	{
	  newval = Grob_array::make_array ();
	  sc->set_object (sym, newval);
	}

      Grob_array *new_array = unsmob_grob_array (newval);
      for (int k = 0; k < 2;k++)
	for (int j = (*arrs[k])[i][LEFT]; j <= (*arrs[k])[i][RIGHT]; j++)
	  {
	    Grob *substituted = substitute_grob (vec[j].grob_);
	    if (substituted)
	      new_array->add (substituted);
	  }

#ifdef PARANOIA
      printf ("%d (%d), sp %d (%d)\n",
	      item_indices [i].length (), item_index,
	      spanner_indices[i].length (), len -spanner_index);

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
SCM
substitute_object_alist (SCM alist, SCM dest)
{
  SCM l = SCM_EOL;
  SCM *tail = &l;
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM sym = scm_caar (s);
      SCM val = scm_cdar (s);

      if (Grob_array *orig = unsmob_grob_array (val))
	{
	  SCM handle = scm_assq (sym, dest);
	  SCM newval
	    = (scm_is_pair (handle))
	    ? scm_cdr (handle)
	    : Grob_array::make_array ();

	  Grob_array *new_arr = unsmob_grob_array (newval);

	  substitute_grob_array (orig, new_arr);
	  val = newval;
	}
      else
	val = do_break_substitution (val);

      if (val != SCM_UNDEFINED)
	{
	  /*
	    for ly:grob? properties, SCM_UNDEFINED could leak out
	    through ly:grob-property
	  */
	  *tail = scm_cons (scm_cons (sym, val), SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	}
    }
  return l;
}

void
Spanner::substitute_one_mutable_property (SCM sym,
					  SCM val)
{
  Spanner *s = this;

  bool fast_done = false;
  Grob_array *grob_array = unsmob_grob_array (val);
  if (grob_array)
    fast_done = s->fast_substitute_grob_array (sym, grob_array);

  if (!fast_done)
    for (vsize i = 0; i < s->broken_intos_.size (); i++)
      {
	Grob *sc = s->broken_intos_[i];
	System *l = sc->get_system ();
	set_break_subsititution (l ? l->self_scm () : SCM_UNDEFINED);

	if (grob_array)
	  {
	    SCM newval = sc->internal_get_object (sym);
	    if (!unsmob_grob_array (newval))
	      {
		newval = Grob_array::make_array ();
		sc->set_object (sym, newval);
	      }
	    substitute_grob_array (grob_array, unsmob_grob_array (newval));
	  }
	else
	  {
	    SCM newval = do_break_substitution (val);
	    sc->set_object (sym, newval);
	  }
      }
}

void
Grob::substitute_object_links (SCM crit, SCM orig)
{
  set_break_subsititution (crit);
  object_alist_ = substitute_object_alist (orig, object_alist_);
}
