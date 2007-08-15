/*
  object-key-dumper-scheme.cc -- implement Object_key_dumper bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "object-key-dumper.hh"

#include "moment.hh"

LY_DEFINE (ly_make_dumper, "ly:make-dumper",
	   0, 0, 0,
	   (),
	   "Create a key dumper. ")
{
  Object_key_dumper *u = new Object_key_dumper ();
  return u->unprotect ();
}

LY_DEFINE (ly_dumper_definitions, "ly:dumper-definitions",
	   1, 0, 0,
	   (SCM dumper),
	   "Return list of key definitions. ")
{
  Object_key_dumper *u = unsmob_key_dumper (dumper);
  SCM_ASSERT_TYPE (u, dumper, SCM_ARG1, __FUNCTION__, "dumper");
  return u->get_file_contents ();
}

LY_DEFINE (ly_dumper_key_serial, "ly:dumper-key-serial",
	   2, 0, 0,
	   (SCM dumper, SCM key),
	   "Return the  key serial number @var{key}. ")
{
  Object_key_dumper *u = unsmob_key_dumper (dumper);
  Object_key *k = unsmob_key (key);
  SCM_ASSERT_TYPE (u, dumper, SCM_ARG1, __FUNCTION__, "dumper");
  SCM_ASSERT_TYPE (k, key, SCM_ARG2, __FUNCTION__, "key");
  return u->dump_key (k);
}
