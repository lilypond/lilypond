/*
  lily-version.cc -- implement version strings

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lily-version.hh"

#include "config.hh"
#include "version.hh"

string
version_string ()
{
  string str = MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL;
  string mpl ("." MY_PATCH_LEVEL);
  if (mpl != ".")
    str += mpl;
  return str;
}

string
gnu_lilypond_string ()
{
  string str = "GNU LilyPond";
  return str;
}

string
gnu_lilypond_version_string ()
{
  string str = gnu_lilypond_string () + " " + version_string ();
  return str;
}

