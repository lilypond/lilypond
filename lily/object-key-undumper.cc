/*
  object-key-undumper.cc --  implement Object_key_undumper

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include <map>


#include "smobs.hh"
#include "object-key.hh"
#include "object-key-undumper.hh"

#include "ly-smobs.icc"

IMPLEMENT_SMOBS(Object_key_undumper);
IMPLEMENT_DEFAULT_EQUAL_P(Object_key_undumper);

SCM
Object_key_undumper::mark_smob (SCM smob)
{
  Object_key_undumper * undumper = (Object_key_undumper*) SCM_CELL_WORD_1(smob);
  for (Int_to_key_map::const_iterator i (undumper->keys_.begin());
       i != undumper->keys_.end(); i++)
    {
      scm_gc_mark ((*i).second->self_scm ());
    }

  return SCM_BOOL_F;
}

int
Object_key_undumper::print_smob (SCM s, SCM port, scm_print_state*)
{
  scm_puts ("#<Object_key_undumper>", port);
  return 1;
}


Object_key_undumper::Object_key_undumper (SCM s)
{
  smobify_self();
  parse_contents (s);
}


LY_DEFINE(ly_make_undumper, "ly:make-undumper",
	  1,0,0,
	  (SCM contents),
	  "Create a key undumper for @var{contents}. "
	  )
{
  Object_key_undumper *u = new Object_key_undumper (contents);
  SCM x = u->self_scm();
  scm_gc_unprotect_object (x);
  return x;
}


LY_DEFINE(ly_undumper_lookup, "ly:undumper-lookup",
	  2,0,0,
	  (SCM undumper, SCM serial),
	  "Return the object key for number @var{serial}. "
	  )
  
{
  Object_key_undumper* u = unsmob_key_undumper (undumper);

  SCM_ASSERT_TYPE(u, undumper, SCM_ARG1, __FUNCTION__, "undumper");
  SCM_ASSERT_TYPE(scm_is_integer(serial), serial, SCM_ARG2, __FUNCTION__, "integer");
  return u->get_key (scm_to_int (serial))->self_scm();
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
	  SCM entry = scm_car (t);
	  if (scm_is_pair (entry)
	      && scm_car (entry) == ly_symbol2scm ("key"))
	    {
	      int index = scm_to_int (scm_cadr (entry));
	      Object_key const *key = get_key (index);
	      *tail = scm_cons (key->self_scm(), SCM_EOL);
	    }
	  else
	    {
	      *tail = scm_cons (entry, SCM_EOL);
	    }
	  tail = SCM_CDRLOC(*tail);
	}

      Object_key *k = Object_key::undump (new_key);
      keys_[number] = k;
      scm_gc_unprotect_object (k->self_scm());
    }
  
}

Object_key const* 
Object_key_undumper::get_key (int idx)
{
  Int_to_key_map::const_iterator i (keys_.find (idx));
  assert (i != keys_.end());

  return (*i).second;
}

Object_key_undumper::~Object_key_undumper()
{
}
