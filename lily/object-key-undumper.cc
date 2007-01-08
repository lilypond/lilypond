/*
  object-key-undumper.cc -- implement Object_key_undumper

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "object-key-undumper.hh"

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Object_key_undumper);
IMPLEMENT_DEFAULT_EQUAL_P (Object_key_undumper);

SCM
Object_key_undumper::mark_smob (SCM smob)
{
  Object_key_undumper *undumper = (Object_key_undumper *) SCM_CELL_WORD_1 (smob);
  for (Int_to_key_map::const_iterator i (undumper->keys_.begin ());
       i != undumper->keys_.end (); i++)
    scm_gc_mark ((*i).second->self_scm ());

  return SCM_BOOL_F;
}

int
Object_key_undumper::print_smob (SCM s, SCM port, scm_print_state*)
{
  (void) s;
  scm_puts ("#<Object_key_undumper>", port);
  return 1;
}

Object_key_undumper::Object_key_undumper ()
{
  smobify_self ();
}

void
Object_key_undumper::parse_contents (SCM contents)
{
  for (SCM s = contents; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM entry = scm_car (s);
      if (scm_car (entry) != ly_symbol2scm ("define-key"))
	continue;

      int number = scm_to_int (scm_cadr (entry));
      SCM skey = scm_caddr (entry);

      SCM new_key = SCM_EOL;
      SCM *tail = &new_key;
      for (SCM t = skey; scm_is_pair (t); t = scm_cdr (t))
	{
	  SCM item = scm_car (t);
	  if (scm_is_pair (item)
	      && scm_car (item) == ly_symbol2scm ("key"))
	    {
	      int index = scm_to_int (scm_cadr (item));
	      Object_key const *key = get_key (index);
	      *tail = scm_cons (key->self_scm (), SCM_EOL);
	    }
	  else
	    *tail = scm_cons (item, SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	}

      Object_key *k = Object_key::undump (new_key);
      keys_[number] = k;
      k->unprotect ();
    }
}

Object_key const *
Object_key_undumper::get_key (int idx)
{
  Int_to_key_map::const_iterator i (keys_.find (idx));
  assert (i != keys_.end ());

  return (*i).second;
}

Object_key_undumper::~Object_key_undumper ()
{
}
