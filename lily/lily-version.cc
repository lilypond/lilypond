/*
  lily-version.cc -- implement version strings

  source file of the GNU LilyPond music typesetter

  (c)  1999--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "config.h"
#include "version.hh"
#include "lily-version.hh"

String
version_str ()
{
  String str = MAJOR_VERSION "." MINOR_VERSION "."  PATCH_LEVEL;
  String mpl ("." MY_PATCH_LEVEL);
  if (mpl != ".")
    str += mpl;
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

