/*
  object-key-dumper.cc --  implement Object_key_dumper

  source file of the GNU LilyPond music typesetter

  (c) 2004 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/


#include <map>

#include "ly-smobs.icc"

#include "moment.hh"
#include "object-key-dumper.hh"
#include "object-key.hh"


SCM
Object_key_dumper::mark_smob (SCM smob )
{
  Object_key_dumper * dumper = (Object_key_dumper*) SCM_CELL_WORD_1 (smob);
  
  for (Key_to_key_map::const_iterator i (dumper->serialized_keys_.begin ());
       i != dumper->serialized_keys_.end();
       i++)
    {
      scm_gc_mark ((*i).first->self_scm());
    }
  return SCM_EOL;
}

int
Object_key_dumper::print_smob (SCM, SCM port, scm_print_state*)
{
  scm_puts ("#<Object_key_dumper>", port);
  return 1;
}

IMPLEMENT_DEFAULT_EQUAL_P(Object_key_dumper);
IMPLEMENT_SMOBS(Object_key_dumper);

Object_key_dumper::Object_key_dumper ()
{
  file_contents_ = SCM_EOL;
  next_available_ = 0;
  smobify_self ();
}

SCM
Object_key_dumper::key_serial (int k)
{
  return scm_list_2 (ly_symbol2scm ("key"),
		     scm_from_int (k));
}

SCM
Object_key_dumper::serialize_key (Object_key const *key)
{
  SCM skey = key->dump();
  for (SCM s = skey ; scm_is_pair (s) ; s = scm_cdr (s))
    {
      if (Object_key const * sub_key = unsmob_key (scm_car (s)))
	{
	  scm_set_car_x (s, dump_key (sub_key));
	}
      else if (Moment *mom = unsmob_moment (scm_car (s)))
	{
	  scm_set_car_x (s,
			 scm_list_2 (ly_symbol2scm ("unquote"),
				     mom->as_scheme()));
	}
    }

  file_contents_ = scm_cons (scm_list_3 (ly_symbol2scm("define-key"),
					 scm_from_int (next_available_),
					 skey),
			     file_contents_);

  serialized_keys_[key] = key;
  key_serial_numbers_[key] = next_available_;
  SCM retval = key_serial (next_available_);
  next_available_ ++;

  return retval;
}

SCM
Object_key_dumper::dump_key (Object_key const *key)
{
  if (key_serial_numbers_.find (key) != key_serial_numbers_.end ())
    {
      return key_serial (key_serial_numbers_[key]);
    }
  else if (Object_key const *serialized = serialized_keys_[key])
    {
      return key_serial (key_serial_numbers_[ serialized_keys_ [serialized] ]);
    }
  
  return serialize_key (key);
}

SCM
Object_key_dumper::get_file_contents () const
{
  return scm_reverse (file_contents_);
}

LY_DEFINE(ly_make_dumper, "ly:make-dumper",
	  0,0,0,
	  (),
	  "Create a key dumper. "
	  )
{
  Object_key_dumper *u = new Object_key_dumper ();
  SCM x = u->self_scm();
  scm_gc_unprotect_object (x);
  return x;
}

LY_DEFINE(ly_dumper_definitions, "ly:dumper-definitions",
	  1,0,0,
	  (SCM dumper),
	  "Return list of key definitions. "
	  )
{
  Object_key_dumper *u = unsmob_key_dumper (dumper);
  SCM_ASSERT_TYPE(u, dumper, SCM_ARG1, __FUNCTION__, "dumper");
  return u->get_file_contents();
}

LY_DEFINE(ly_dumper_key_serial, "ly:dumper-key-serial",
	  2,0,0,
	  (SCM dumper, SCM key),
	  "Return the  key serial number @var{key}. "
	  )
{
  Object_key_dumper* u = unsmob_key_dumper (dumper);
  Object_key *k = unsmob_key (key);
  SCM_ASSERT_TYPE(u, dumper, SCM_ARG1, __FUNCTION__, "dumper");
  SCM_ASSERT_TYPE(k, key, SCM_ARG2, __FUNCTION__, "key");
  return u->dump_key (k);
}

Object_key_dumper::~Object_key_dumper()
{
}
