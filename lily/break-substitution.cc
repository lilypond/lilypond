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
      if (sc->line_l () != line)
	{
	  sc = sc->find_broken_piece (line);

	}
	  
      /* now: !sc || (sc && sc->line_l () == line) */
      if (!sc)
	return SCM_UNDEFINED;

      /* now: sc && sc->line_l () == line */
      if (!line)
	return sc->self_scm();


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


SCM grob_list_p; 

/*
  Although the substitution can be written as

  mutable_property_alist_ = do_substitution (mutable_property_alist_),

  we have a special function here: we want to invoke a special
  function for lists of grobs. These can be very long for large
  orchestral scores (eg. 1M elements). do_break_substitution() can
  recurse many levels, taking lots of stack space.

  This becomes a problem if lily is linked against guile with
  pthreads. pthreads impose small limits on the stack size.
 */
SCM
substitute_mutable_properties (SCM alist)
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
