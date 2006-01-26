/*
  lily-version.cc -- implement version strings

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lily-version.hh"

#include "config.hh"
#include "version.hh"

std::string
version_string ()
{
  std::string str = MAJOR_VERSION "." MINOR_VERSION "." PATCH_LEVEL;
  std::string mpl ("." MY_PATCH_LEVEL);
  if (mpl != ".")
    str += mpl;
  return str;
}

std::string
gnu_lilypond_string ()
{
  std::string str = "GNU LilyPond";
  return str;
}

std::string
gnu_lilypond_version_string ()
{
  std::string str = gnu_lilypond_string () + " " + version_string ();
  return str;
}

