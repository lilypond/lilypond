/*
  lily-version.cc -- implement version strings

  source file of the GNU LilyPond music typesetter

  (c)  1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "config.hh"
#include "version.hh"
#include "lily-version.hh"

#define VERSION MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL "." MY_PATCH_LEVEL

String
version_str ()
{
  String str = VERSION;
  return str;
}

String 
gnu_lilypond_str ()
{
  String str = "GNU LilyPond";
  return str;
}

String 
gnu_lilypond_version_str ()
{
  String str = gnu_lilypond_str () + " " + version_str ();
  return str;
}

