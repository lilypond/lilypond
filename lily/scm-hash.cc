/*   
  scm-hash.cc --  implement Scheme_hash_table
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <stdio.h>

#include "scm-hash.hh"
#include "ly-smobs.icc"


Scheme_hash_table::Scheme_hash_table ()
{
  smobify_self ();
}


Scheme_hash_table::Scheme_hash_table (Scheme_hash_table const &src)
  : Scm_stl_map (src)
{
  smobify_self ();
}

void
Scheme_hash_table::operator =(Scheme_hash_table const & src)
{
  Scm_stl_map::operator = (src);
	
  // we do not copy the self_scm () field!
}

SCM
Scheme_hash_table::mark_smob (SCM s)
{
  /*
    can't typecheck naively, since GC bit lives in CAR of S
   */
  
  Scheme_hash_table *me = (Scheme_hash_table*) SCM_CELL_WORD_1(s);

  for (Scm_stl_map::const_iterator i= me->begin (); i != me->end(); i++)
    {
      scm_gc_mark ((*i).first);
      scm_gc_mark ((*i).second);
    }
  return SCM_EOL;
}

int
Scheme_hash_table::print_smob (SCM s, SCM p, scm_print_state*)
{
  assert (unsmob (s));
  char str[1000];
  sprintf (str, "#<Scheme_hash_table 0x%0x ", s);
  scm_puts (str, p);      
  Scheme_hash_table *me = unsmob(s);
  for (Scm_stl_map::const_iterator i = me->begin (); i != me->end(); i++)
    {
      scm_display ((*i).first, p);
      scm_puts (" = ",p);      
      scm_display ((*i).second, p);
      scm_puts ("\n",p);            
    }
  scm_puts ("> ",p);        
  return 1;
}

bool
Scheme_hash_table::try_retrieve (SCM k, SCM *v)
{
  Scm_stl_map ::const_iterator i (find (k));
  bool found = i != end ();
  if (found)
    *v = (*i).second;
  return found;
}

bool
Scheme_hash_table::elem_b (SCM k) const
{
  Scm_stl_map::const_iterator i (find (k));
  return i != end ();
}

void
Scheme_hash_table::set (SCM k, SCM v)
{
  (*this)[k] = v;
}

// UGH. 
SCM
Scheme_hash_table::get (SCM k)const
{
  return (*(Scheme_hash_table*)this)[k]; 
}


Scheme_hash_table::~Scheme_hash_table( )
{
}

SCM
Scheme_hash_table::to_alist () const
{
  SCM l = SCM_EOL;
  for (Scm_stl_map ::const_iterator i = begin (); i != end(); i++)
    l = gh_cons (gh_cons ((*i).first, (*i).second), l);
  return l;  
}



IMPLEMENT_UNSMOB(Scheme_hash_table,scheme_hash);
IMPLEMENT_SMOBS(Scheme_hash_table);
IMPLEMENT_DEFAULT_EQUAL_P(Scheme_hash_table);


