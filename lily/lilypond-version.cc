/*
  lilypond-version.cc -- implement Lilypond_version

  source file of the GNU LilyPond music typesetter

  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#include "lilypond-input-version.hh"
#include "string-convert.hh"
#include "array.hh"

Lilypond_version::Lilypond_version (int major, int minor, int patch)
{
  major_ = major;
  minor_ = minor;
  patch_ = patch;
}

Lilypond_version::Lilypond_version (String str)
{
  Array<String> version;
  version = String_convert::split (str, '.');
  
  major_ = version[0].to_int ();
  minor_ = version[1].to_int ();
  patch_ = 0;
  if (version.size () >= 3)
    patch_ = version[2].to_int ();

  if (version.size () >= 4)
    extra_patch_string_ = version[3];
}

String
Lilypond_version::string () const
{
  return to_string (major_) + "." + to_string (minor_) + "." + to_string (patch_);
}

Lilypond_version::operator int () const
{
    // ugh
  return 100000 * major_ + 1000 * minor_ + patch_;
}

