/*
  scm-hash.cc -- implement Scheme_hash_table

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "scm-hash.hh"

#include <cstdio>
#include <algorithm>
using namespace std;

#include "ly-smobs.icc"

/*
  Return: number of objects.
*/
int
copy_scm_hashes (SCM dest, SCM src)
{
  int k = 0;
  for (int i = scm_c_vector_length (src); i--;)
    for (SCM s = scm_vector_ref (src, scm_from_int (i)); scm_is_pair (s); s = scm_cdr (s))
      {
	scm_hashq_set_x (dest, scm_caar (s), scm_cdar (s));
	k++;
      }
  return k;
}

Scheme_hash_table::Scheme_hash_table ()
{
  hash_tab_ = SCM_EOL;
  smobify_self ();
  hash_tab_ = scm_make_vector (scm_from_int (119), SCM_EOL);
  elt_count_ = 0;
}

Scheme_hash_table::Scheme_hash_table (Scheme_hash_table const &src)

{
  hash_tab_ = SCM_EOL;
  elt_count_ = 0;
  smobify_self ();

  hash_tab_ = scm_make_vector (scm_from_int (max ((int) src.elt_count_, 11)), SCM_EOL);
  elt_count_ = copy_scm_hashes (hash_tab_, src.hash_tab_);
}

void
Scheme_hash_table::operator = (Scheme_hash_table const &src)
{
  if (&src == this)
    return;

  hash_tab_ = scm_make_vector (scm_from_int (max ((int) src.elt_count_, 11)), SCM_EOL);
  elt_count_ = copy_scm_hashes (hash_tab_, src.hash_tab_);
}

Scheme_hash_table::~Scheme_hash_table ()
{
}

SCM
Scheme_hash_table::mark_smob (SCM s)
{
  Scheme_hash_table *me = (Scheme_hash_table *) SCM_CELL_WORD_1 (s);
  scm_gc_mark (me->hash_tab_);
  return SCM_EOL;
}

int
Scheme_hash_table::print_smob (SCM s, SCM p, scm_print_state*)
{
  assert (unsmob (s));
  scm_puts ("#<Scheme_hash_table  ", p);
  Scheme_hash_table *me = (Scheme_hash_table *) SCM_CELL_WORD_1 (s);
  scm_display (me->hash_tab_, p);
  scm_puts ("> ", p);
  return 1;
}

bool
Scheme_hash_table::try_retrieve (SCM k, SCM *v){

  SCM handle = scm_hashq_get_handle (hash_tab_, k);
  if (scm_is_pair (handle))
    {
      *v = scm_cdr (handle);
      return true;
    }
  else
    return false;
}

bool
Scheme_hash_table::contains (SCM k) const
{
  return scm_is_pair (scm_hashq_get_handle (hash_tab_, k));
}

void
Scheme_hash_table::set (SCM k, SCM v)
{
  assert (scm_is_symbol (k));
  SCM handle = scm_hashq_create_handle_x (hash_tab_, k, SCM_UNDEFINED);
  if (scm_cdr (handle) == SCM_UNDEFINED)
    elt_count_++;

  scm_set_cdr_x (handle, v);

  /*
    resize if getting too large.
  */
  if (elt_count_ > 2 * scm_c_vector_length (hash_tab_))
    {
      SCM nh = scm_make_vector (scm_from_int (3 * elt_count_ + 1), SCM_EOL);
      elt_count_ = copy_scm_hashes (nh, hash_tab_);
      hash_tab_ = nh;
    }
}

// UGH.
SCM
Scheme_hash_table::get (SCM k) const
{
  /*
    42 will stick out like a sore thumb, hopefully.
  */
  return scm_hashq_ref (hash_tab_, k, scm_from_int (42));
}

void
Scheme_hash_table::remove (SCM k)
{
  scm_hashq_remove_x (hash_tab_, k);
  /* Do not decrease elt_count_ as this may cause underflow.  The exact
     value of elt_count_ is not important. */
}

SCM
Scheme_hash_table::to_alist () const
{
  SCM lst = SCM_EOL;
  for (int i = scm_c_vector_length (hash_tab_); i--;)
    for (SCM s = scm_vector_ref (hash_tab_, scm_from_int (i)); scm_is_pair (s);
	 s = scm_cdr (s))
      lst = scm_acons (scm_caar (s), scm_cdar (s), lst);
  return lst;
}

IMPLEMENT_SMOBS (Scheme_hash_table);
IMPLEMENT_DEFAULT_EQUAL_P (Scheme_hash_table);
