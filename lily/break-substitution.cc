#include <stdio.h>

#include  "grob.hh"
#include "item.hh"
#include  "spanner.hh"
#include  "system.hh"

static SCM break_criterion; 
void
set_break_subsititution (SCM criterion)
{
  break_criterion = criterion;
}

/*
  Perform the substitution for a single grob.   
 */
SCM
substitute_grob (Grob *sc)
{
  if (SCM_INUMP (break_criterion))
    {
      Item * i = dynamic_cast<Item*> (sc);
      Direction d = to_dir (break_criterion);
      if (i && i->break_status_dir () != d)
	{
	  Item *br = i->find_prebroken_piece (d);
	  return (br) ? br->self_scm () : SCM_UNDEFINED;
	}
    }
  else
    {
      System * line
	= dynamic_cast<System*> (unsmob_grob (break_criterion));
      if (sc->get_system () != line)
	{
	  sc = sc->find_broken_piece (line);

	}
	  
      /* now: !sc || (sc && sc->get_system () == line) */
      if (!sc)
	return SCM_UNDEFINED;

      /* now: sc && sc->get_system () == line */
      if (!line)
	return sc->self_scm();

      /*
	We don't return SCM_UNDEFINED for
	suicided grobs, for two reasons

	- it doesn't work (strange disappearing objects)

	- it forces us to mark the parents of a grob, leading to
	a huge recursion in the GC routine.
       */

      /*
	This was introduced in 1.3.49 as a measure to prevent
	programming errors. It looks rather expensive (?).

	TODO:
		
	benchmark , document when (what kind of programming
	errors) this happens.
      */
      if (sc->common_refpoint (line, X_AXIS)
	  && sc->common_refpoint (line, Y_AXIS))
	{
	  return sc->self_scm ();
	}
      return SCM_UNDEFINED;
    }

  return sc->self_scm();
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
      return substitute_grob (unsmob_grob (src));
    }
  else if (gh_vector_p (src))
    {
      int  l = SCM_VECTOR_LENGTH (src);
      SCM nv = scm_c_make_vector (l, SCM_UNDEFINED);

      for (int i  =0 ; i< l ; i++)
	{
	  SCM si = scm_int2num (i);
	  scm_vector_set_x (nv, si, do_break_substitution (scm_vector_ref (src, si))); 
	}
    }
  else if (ly_pair_p (src)) 
    {
      /*
	UGH! breaks on circular lists.
      */
      SCM newcar = do_break_substitution (ly_car (src));
      SCM oldcdr = ly_cdr (src);
      
      if (newcar == SCM_UNDEFINED
	  && (gh_pair_p (oldcdr) || oldcdr == SCM_EOL))
	{
	  /*
	    This is tail-recursion, ie. 
	    
	    return do_break_substution (cdr);

	    We don't want to rely on the compiler to do this.  Without
	    tail-recursion, this easily crashes with a stack overflow.  */
	  src =  oldcdr;
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
SCM
substitute_grob_list (SCM grob_list)
{
  SCM l = SCM_EOL;
  SCM * tail = &l;

  for (SCM s = grob_list; gh_pair_p (s); s =  gh_cdr (s))
    {
      SCM n= substitute_grob (unsmob_grob (gh_car (s)));

      if (n != SCM_UNDEFINED)
	{
	  *tail = gh_cons (n, SCM_EOL);
	  tail = SCM_CDRLOC(*tail);
	}
    }

  return l;
}

/*
  We don't do

  forall b in broken-childs:
     forall p in properties:
        forall g in p (if grob-list):
	  g := substitute (g)

  for spanners since this is O(SYSTEMCOUNT * GROBCOUNT), and SYSTEMCOUNT =
  O(GROBCOUNT), we have a quadratic algorithm. --for a single spanner

  This is problematic: with large (long) scores, the costs can be
  significant; especially all-elements in System, can become huge. For
  a typical 50 page score, it requires running through a 100k list 50
  times.
  
  Instead:

  forall p in properties:
     (if grob list)  

     put  grob list in array,

     reorder array so spanners are separate -- O(grobcount)
     
     find first and last indexes of grobs on a specific system

     for items this is O(itemcount)

     for spanners this is O(sum-of spanner-system-ranges)

     perform the substitution O(sum-of spanner-system-ranges)


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
spanner_system_range (Spanner* sp)
{
  Slice rv;
  
  if (System*st = sp->get_system())
    {
      rv = Slice (st->rank_, st->rank_);
    }
  else 
    {
      if (sp->broken_intos_.size())
	rv = Slice (sp->broken_intos_[0]->get_system()->rank_,
		    sp->broken_intos_.top()->get_system()->rank_);
    }
  return rv;
}

Slice
item_system_range (Item* it)
{
  if (System*st= it->get_system())
    return Slice (st->rank_, st->rank_);

  Slice sr;
  Direction d = LEFT;
  do
    {
      Item *bi = it->find_prebroken_piece (d);
      if (bi && bi->get_system())
	sr.add_point (bi->get_system()->rank_);
    }
  while (flip(&d)!=LEFT);
  
  return sr;
}

Slice
grob_system_range (Grob *g)
{
 if (Spanner*s = dynamic_cast<Spanner*>(g))
   return spanner_system_range (s);
 else if (Item* it = dynamic_cast<Item*> (g))
   return item_system_range (it);
 else
   return Slice();
}



struct Substitution_entry
{
  Grob * grob_;
  short left_;
  short right_;
  
  void set (Grob*g, Slice sr)
  {
    grob_ = g;
    /*
      duh, don't support scores with more than 32000 systems.
    */
    if (sr.empty_b())
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
  Substitution_entry()
  {
    grob_ =0;
    left_ = right_ = -2;
  }
  
  int length () { return right_ - left_ ; }
  static int
  item_compare (void const * a , void const * b)
  {
    return ((Substitution_entry*)a)->left_ -
      ((Substitution_entry*)b)->left_;
  }
 
  static int
  spanner_compare (void const * a , void const * b)
  {
    return ((Substitution_entry*)a)->length() -
      ((Substitution_entry*)b)->length ();
  }
};
  

    
bool
Spanner::fast_fubstitute_grob_list (SCM sym,
				    SCM grob_list)
{
  int len = scm_ilength (grob_list);

  /*
    Only do this complicated thing for large lists. This has the added
    advantage that we won't screw up the ordering for elements in
    alignments (which typically don't have more than 100 grobs.)
   */
  
  if (len < 100)
    return false;


  /*
    TODO : should not reallocate every time? 
   */
  static Substitution_entry * vec;
  static int vec_room;

  if (vec_room < len)
    {
      vec = (Substitution_entry*) scm_realloc (vec, sizeof (Substitution_entry) * len);
      vec_room = len;
    }
  
  Slice system_range = spanner_system_range (this);

  Array<Slice> it_indices;
  Array<Slice> sp_indices;
  for (int i = 0; i <= system_range.length (); i++)
    {
      it_indices.push (Slice (len, 0));
      sp_indices.push (Slice (len, 0));
    }
  
  
  int sp_index = len;
  int it_index = 0;
  for (SCM s = grob_list; gh_pair_p (s); s = gh_cdr (s))
    {
      Grob * g = unsmob_grob (gh_car(s));

      Slice sr = grob_system_range (g);
      sr.intersect (system_range);

      int idx = 0;
      if (dynamic_cast<Spanner*>(g))
	{
	  idx =--sp_index;
	}
      else if (dynamic_cast<Item*> (g))
	{
	  idx = it_index++;
	}

      vec[idx].set (g, sr);
    }

  qsort (vec, it_index,
	 sizeof (Substitution_entry), &Substitution_entry::item_compare);

 Array<Slice> *arrs[] = {
	&it_indices, &sp_indices
 };
        
 for (int i = 0; i < it_index ;i++)
   {
     for (int j = vec[i].left_; j <= vec[i].right_; j++)
       {
	 it_indices[j - system_range[LEFT]].add_point (i);
       }
   }

#if 0
  qsort (vec + sp_index, len - sp_index,
	 sizeof (Substitution_entry), &Substitution_entry::spanner_compare);
 /*
   This is a waste of time -- the staff-spanners screw up the
   ordering, since they go across the entire score.
 */
 for (int i = sp_index; i < len ;i++)
   {
     
     for (int j = vec[i].left_; j <= vec[i].right_; j++)
       {
	 sp_indices[j - system_range[LEFT]].add_point (i);
       }
   }
#else
 for (int i = sp_indices.size(); i--;)
   sp_indices[i]= Slice (sp_index, len-1);
#endif

 
  assert (it_index <= sp_index);

  assert (broken_intos_.size () == system_range.length () + 1); 
  for (int i = 0; i < broken_intos_.size(); i++)
    {
      Grob * sc = broken_intos_[i];
      System * l = sc->get_system ();
      set_break_subsititution (l ? l->self_scm(): SCM_UNDEFINED);

      SCM newval = SCM_EOL;
      SCM * tail = &newval;

     for (int k = 0; k < 2;k++)
	for (int j = (*arrs[k])[i][LEFT]; j <= (*arrs[k])[i][RIGHT]; j++)
	  {
	    SCM subs =substitute_grob (vec[j].grob_);
	    if (subs!= SCM_UNDEFINED)
	      {
		*tail = scm_cons (subs, SCM_EOL);
		
		tail = SCM_CDRLOC(*tail);
	      }

	  }
	      
#ifdef PARANOIA
     
      printf ("%d (%d), sp %d (%d)\n",
	      it_indices [i].length (), it_index,
	      sp_indices[i].length() , len -sp_index);
	      
      {
	SCM l1 =substitute_grob_list (grob_list);
	assert (scm_ilength (l1) == scm_ilength (newval));
      }
#endif

      sc->mutable_property_alist_ = scm_acons (sym, newval,
					       sc->mutable_property_alist_);
    }

  return true;
}


SCM grob_list_p; 

/*
  Although the substitution can be written as

  property_alist = do_substitution (other_property_alist),

  we have a special function here: we want to invoke a special
  function for lists of grobs. These can be very long for large
  orchestral scores (eg. 1M elements). do_break_substitution() can
  recurse many levels, taking lots of stack space.

  This becomes a problem if lily is linked against guile with
  pthreads. pthreads impose small limits on the stack size.
 */
SCM
substitute_mutable_property_alist (SCM alist)
{
  if (!grob_list_p)
    grob_list_p = scm_c_eval_string ("grob-list?");

  SCM l = SCM_EOL;
  SCM *tail = &l;
  for (SCM s = alist; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM sym = gh_caar(s);
      SCM val = gh_cdar(s);
      SCM type = scm_object_property (sym, ly_symbol2scm ("backend-type?"));

      if (type == grob_list_p)
	val = substitute_grob_list (val);
      else
	val = do_break_substitution (val);

      *tail = gh_cons (gh_cons (sym, val), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  return l;
}


void
Spanner::substitute_one_mutable_property (SCM sym,
					  SCM val)
{
  SCM type = scm_object_property (sym, ly_symbol2scm ("backend-type?"));
  Spanner*s = this;
  
  bool fast_done = false;
  if (type == grob_list_p)
    fast_done = s->fast_fubstitute_grob_list (sym, val);

  if (!fast_done)  
    for (int i = 0; i < s->broken_intos_ .size (); i++)
      {
	Grob * sc = s->broken_intos_[i];
	System * l = sc->get_system ();
	set_break_subsititution (l ? l->self_scm () : SCM_UNDEFINED);

	SCM newval = (type == grob_list_p)
	  ? substitute_grob_list (val)
	  : do_break_substitution(val);

	sc->mutable_property_alist_ = scm_cons (scm_cons (sym, newval),
						sc->mutable_property_alist_);
      }
}
				       
