/*
  scm-hash.cc -- implement Scheme_hash_table

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "scm-hash.hh"

#include <cstdio>
#include <algorithm>
using namespace std;

#include "ly-smobs.icc"

/*
  Return: number of objects.
*/
SCM
copy_handle (void *closure, SCM handle)
{
  SCM tab = (SCM) closure;
  scm_hashq_set_x (tab, scm_car (handle), scm_cdr (handle));
  return tab;
}

static void
copy_scm_hashes (SCM dest, SCM src)
{
  scm_internal_hash_for_each_handle (  (SCM (*)(GUILE_ELLIPSIS)) &copy_handle, dest, src);
}

Scheme_hash_table::Scheme_hash_table ()
{
  hash_tab_ = SCM_EOL;
  smobify_self ();
  hash_tab_ = scm_c_make_hash_table (119);
}

Scheme_hash_table::Scheme_hash_table (Scheme_hash_table const &src)
{
  hash_tab_ = SCM_EOL;
  smobify_self ();
  copy (src);
}

void
Scheme_hash_table::copy (Scheme_hash_table const &src)
{
  if (&src == this)
    return;

  hash_tab_ = scm_c_make_hash_table (SCM_HASHTABLE_N_ITEMS(src.hash_tab_));
  copy_scm_hashes (hash_tab_, src.hash_tab_);
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
  scm_set_cdr_x (handle, v);
}

SCM
Scheme_hash_table::get (SCM k) const
{
  /* SCM_UNSPECIFIED will stick out like a sore thumb, hopefully.
  */
  return scm_hashq_ref (hash_tab_, k, SCM_UNSPECIFIED);
}

void
Scheme_hash_table::remove (SCM k)
{
  scm_hashq_remove_x (hash_tab_, k);
}

static SCM
collect_handles (void * /* closure */,
		 SCM key,
		 SCM value,
		 SCM result)
{
  return scm_acons(key, value, result);
}

SCM
Scheme_hash_table::to_alist () const
{
  return scm_internal_hash_fold ((SCM (*)(GUILE_ELLIPSIS)) &collect_handles, NULL, SCM_EOL, hash_tab_);
}

IMPLEMENT_SMOBS (Scheme_hash_table);
IMPLEMENT_DEFAULT_EQUAL_P (Scheme_hash_table);
