/*   
  scm-hash.cc --  implement Scheme_hash_table
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <stdio.h>

#include "scm-hash.hh"
#include "hash-table-iter.hh"

Scheme_hash_table::Scheme_hash_table ()
{
  hash_func_ = ly_scm_hash;
  self_scm_ = SCM_EOL;
  smobify_self ();
}

void
Scheme_hash_table::operator =(Scheme_hash_table const & src)
{
  Hash_table<SCM,SCM>::operator = (src);
	
  // we do not copy the self_scm_ field!
}

void
Scheme_hash_table::do_smobify_self ()
{
}

#include "ly-smobs.icc"
IMPLEMENT_SMOBS(Scheme_hash_table);

SCM
Scheme_hash_table::mark_smob (SCM s)
{
  /*
    can't typecheck naively, since GC bit lives in CAR of S
   */
  //assert (SMOB_IS_TYPE_B (Scheme_hash_table, s));
  
  Scheme_hash_table *me = SMOB_TO_TYPE(Scheme_hash_table,s);
  for (Hash_table_iter<SCM,SCM> i (*me); i.ok(); i++)
    {
      scm_gc_mark (i.key());
      scm_gc_mark (i.val ());
    }
  return SCM_EOL;
}


Scheme_hash_table::Scheme_hash_table (Scheme_hash_table const &src)
  : Hash_table<SCM,SCM> (src)
{
  hash_func_ = src.hash_func_;
  self_scm_ = SCM_EOL;
  smobify_self ();
}

int
Scheme_hash_table::print_smob (SCM s, SCM p, scm_print_state*)
{
  assert (SMOB_IS_TYPE_B (Scheme_hash_table, s));
  char str[1000];
  sprintf (str, "#<Scheme_hash_table 0x%0x ", s);
  scm_puts (str, p);      
  Scheme_hash_table *me = SMOB_TO_TYPE(Scheme_hash_table,s);
  for (Hash_table_iter<SCM,SCM> i (*me); i.ok(); i++)
    {
      scm_display (i.key(), p);
      scm_puts (" = ",p);      
      scm_display (i.val (), p);
      scm_puts ("\n",p);            
    }
  scm_puts ("> ",p);        
  return 1;
}


void
Scheme_hash_table::set (SCM k, SCM v)
{
  elem (k ) = v; 
  scm_unprotect_object (v);
}

SCM
Scheme_hash_table::get (SCM k)const
{
  return const_elem (k);
}


Scheme_hash_table::~Scheme_hash_table( )
{
  unsmobify_self ();
}

SCM
Scheme_hash_table::to_alist () const
{
  SCM l = SCM_EOL;
  for (Hash_table_iter<SCM,SCM> i (*this); i.ok(); i++)
    l = gh_cons (gh_cons (i.key (), i.val()), l);
  return l;  
}

