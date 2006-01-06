/*
  lily-version.cc -- implement version strings

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lily-version.hh"

#include "config.hh"
#include "version.hh"

String
version_string ()
{
  String str = MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL;
  String mpl ("." MY_PATCH_LEVEL);
  if (mpl != ".")
    str += mpl;
  return str;
}

String
gnu_lilypond_string ()
{
  String str = "GNU LilyPond";
  return str;
}

String
gnu_lilypond_version_string ()
{
  String str = gnu_lilypond_string () + " " + version_string ();
  return str;
}

