/*
  object-key-dumper.cc -- implement Object_key_dumper

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "object-key-dumper.hh"

#include "moment.hh"

#include "ly-smobs.icc"

SCM
Object_key_dumper::mark_smob (SCM smob)
{
  Object_key_dumper *dumper = (Object_key_dumper *) SCM_CELL_WORD_1 (smob);

  for (Key_to_key_map::const_iterator i (dumper->serialized_keys_.begin ());
       i != dumper->serialized_keys_.end ();
       i++)
    scm_gc_mark ((*i).first->self_scm ());
  return SCM_EOL;
}

int
Object_key_dumper::print_smob (SCM, SCM port, scm_print_state*)
{
  scm_puts ("#<Object_key_dumper>", port);
  return 1;
}

IMPLEMENT_DEFAULT_EQUAL_P (Object_key_dumper);
IMPLEMENT_SMOBS (Object_key_dumper);

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
  SCM skey = key->dump ();
  for (SCM s = skey; scm_is_pair (s); s = scm_cdr (s))
    {
      if (Object_key const *sub_key = unsmob_key (scm_car (s)))
	scm_set_car_x (s, dump_key (sub_key));
      else if (Moment *mom = unsmob_moment (scm_car (s)))
	scm_set_car_x (s,
		       scm_list_2 (ly_symbol2scm ("unquote"),
				   mom->as_scheme ()));
    }

  file_contents_ = scm_cons (scm_list_3 (ly_symbol2scm ("define-key"),
					 scm_from_int (next_available_),
					 skey),
			     file_contents_);

  serialized_keys_[key] = key;
  key_serial_numbers_[key] = next_available_;
  SCM retval = key_serial (next_available_);
  next_available_++;

  return retval;
}

SCM
Object_key_dumper::dump_key (Object_key const *key)
{
  if (key_serial_numbers_.find (key) != key_serial_numbers_.end ())
    return key_serial (key_serial_numbers_[key]);
  else if (Object_key const *serialized = serialized_keys_[key])
    return key_serial (key_serial_numbers_[ serialized_keys_ [serialized] ]);

  return serialize_key (key);
}

SCM
Object_key_dumper::get_file_contents () const
{
  return scm_reverse (file_contents_);
}

Object_key_dumper::~Object_key_dumper ()
{
}
