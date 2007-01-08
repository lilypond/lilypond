/*
  object-key-undumper-scheme.cc -- implement Object_key_undumper bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "object-key-undumper.hh"

LY_DEFINE (ly_undumper_read_keys, "ly:undumper-read-keys",
	   2, 0, 0,
	   (SCM undumper, SCM keys),
	   "Read serialized @var{keys} into @var{undumper}.")
{
  Object_key_undumper *u = unsmob_key_undumper (undumper);
  SCM_ASSERT_TYPE (u, undumper, SCM_ARG1, __FUNCTION__, "Undumper");

  u->parse_contents (keys);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_make_undumper, "ly:make-undumper",
	   0, 0, 0,
	   (),
	   "Create a key undumper. ")
{
  Object_key_undumper *u = new Object_key_undumper ();
  return u->unprotect ();
}

LY_DEFINE (ly_undumper_lookup, "ly:undumper-lookup",
	   2, 0, 0,
	   (SCM undumper, SCM serial),
	   "Return the object key for number @var{serial}. ")

{
  Object_key_undumper *u = unsmob_key_undumper (undumper);

  SCM_ASSERT_TYPE (u, undumper, SCM_ARG1, __FUNCTION__, "undumper");
  SCM_ASSERT_TYPE (scm_is_integer (serial), serial, SCM_ARG2, __FUNCTION__, "integer");
  return u->get_key (scm_to_int (serial))->self_scm ();
}

