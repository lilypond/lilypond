/*
  lilypond-version.cc -- implement Lilypond_version

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <ctype.h>

#include "lilypond-version.hh"
#include "string-convert.hh"
#include "misc.hh"

Lilypond_version::Lilypond_version (int major, int minor, int patch)
{
  major_ = major;
  minor_ = minor;
  patch_ = patch;
}

Lilypond_version::Lilypond_version (string str)
{
  major_ = 0; 
  minor_ = 0;
  patch_ = 0;
  
  vector<string> version;
  version = string_split (str, '.');

  if (version.size () > 0 && isdigit (version[0][0]))
    major_ = String_convert::dec2int (version[0]);
  if (version.size () > 1 && isdigit (version[1][0]))
    minor_ = String_convert::dec2int (version[1]);
  
  patch_ = 0;
  if (version.size () >= 3
      && isdigit (version[2][0]))
    patch_ = String_convert::dec2int (version[2]);

  if (version.size () >= 4)
    extra_patch_string_ = version[3];
}

string
Lilypond_version::to_string () const
{
  return ::to_string (major_)
    + "." + ::to_string (minor_)
    + "." + ::to_string (patch_);
}

Lilypond_version::operator int () const
{
  // ugh
  return 100000 * major_ + 1000 * minor_ + patch_;
}

