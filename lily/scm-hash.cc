/*   
  scm-hash.cc --  implement Scheme_hash_table
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <stdio.h>

#include "scm-hash.hh"
#include "ly-smobs.icc"

void
copy_scm_hashes (SCM dest, SCM src)
{
  for (int i = SCM_SYMBOL_LENGTH (src); i--;)
    for (SCM s = scm_vector_ref (src, SCM_MAKINUM (i)); ly_pair_p(s); s = ly_cdr (s))
      {
	scm_hashq_set_x (dest, ly_caar (s), ly_cdar (s));
      }
}


Scheme_hash_table::Scheme_hash_table ()
{
  hash_tab_ = SCM_EOL;
  smobify_self ();
  hash_tab_ = scm_make_vector (gh_int2scm (119), SCM_EOL);
  elt_count_ = 0;
}


Scheme_hash_table::Scheme_hash_table (Scheme_hash_table const &src)

{
  hash_tab_ = SCM_EOL;
  elt_count_ = src.elt_count_;
  smobify_self ();

  hash_tab_ = scm_make_vector (gh_int2scm (src.elt_count_ >? 11 ), SCM_EOL);  
  copy_scm_hashes (hash_tab_, src.hash_tab_);
}

void
Scheme_hash_table::operator = (Scheme_hash_table const & src)
{
  if (&src == this)
    return;
  
  elt_count_ = src.elt_count_;
  hash_tab_ = scm_make_vector (gh_int2scm (src.elt_count_ >? 11), SCM_EOL);  
  copy_scm_hashes (hash_tab_, src.hash_tab_);
}

SCM
Scheme_hash_table::mark_smob (SCM s)
{
  Scheme_hash_table *me = (Scheme_hash_table*) SCM_CELL_WORD_1 (s);
  scm_gc_mark (me->hash_tab_);
  return SCM_EOL;
}

int
Scheme_hash_table::print_smob (SCM s, SCM p, scm_print_state*)
{
  assert (unsmob (s));
  char str[1000];
  sprintf (str, "#<Scheme_hash_table 0x%0lx ", SCM_UNPACK(s));
   Scheme_hash_table *me = (Scheme_hash_table*) SCM_CELL_WORD_1 (s);
   scm_display (me->hash_tab_, p);      
   scm_puts ("> ",p);        
   return 1;
}

bool
Scheme_hash_table::try_retrieve (SCM k, SCM *v)
{
  SCM handle = scm_hashq_get_handle (hash_tab_, k);
  if (ly_pair_p (handle))
    {
      *v = ly_cdr (handle);
      return true;
    }
  else
    return false;

}

bool
Scheme_hash_table::elem_b (SCM k) const
{
  return ly_pair_p (scm_hashq_get_handle (hash_tab_, k));
}

void
Scheme_hash_table::set (SCM k, SCM v)
{
  assert (gh_symbol_p (k));
  SCM handle = scm_hashq_create_handle_x (hash_tab_, k, SCM_UNDEFINED);
  if (ly_cdr (handle) == SCM_UNDEFINED)
    {
      elt_count_++;
    }
  
  gh_set_cdr_x (handle, v);

  /*
    resize if getting too large.
  */
  if (elt_count_ > 2 * SCM_SYMBOL_LENGTH (hash_tab_))
    {
      SCM nh = scm_make_vector (gh_int2scm (3* elt_count_+1), SCM_EOL);
      copy_scm_hashes (nh, hash_tab_);
      hash_tab_ = nh;
    }
  
}

// UGH. 
SCM
Scheme_hash_table::get (SCM k)const
{
  /*
    42 will stick out like a sore thumb, hopefully.
   */
  return scm_hashq_ref (hash_tab_, k, SCM_MAKINUM(42));
}

void
Scheme_hash_table::remove (SCM k)
{
  scm_hashq_remove_x (hash_tab_, k);
  elt_count_ --;
}

Scheme_hash_table::~Scheme_hash_table ()
{
}

SCM
Scheme_hash_table::to_alist () const
{
  SCM l = SCM_EOL;
  for (int i = SCM_SYMBOL_LENGTH (hash_tab_); i--;)
    for (SCM s = scm_vector_ref (hash_tab_, gh_int2scm (i)); ly_pair_p(s); s = ly_cdr (s))
      {
	l = scm_acons (ly_caar (s), ly_cdar (s), l);
      }
  return l;  
}




IMPLEMENT_UNSMOB (Scheme_hash_table,scheme_hash);
IMPLEMENT_SMOBS (Scheme_hash_table);
IMPLEMENT_DEFAULT_EQUAL_P (Scheme_hash_table);


