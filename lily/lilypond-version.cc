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
  major_i_ = major;
  minor_i_ = minor;
  patch_i_ = patch;
}

Lilypond_version::Lilypond_version (String str)
{
  Array<String> version;
  version = String_convert::split_arr (str, '.');
  
  major_i_ = version[0].value_i ();
  minor_i_ = version[1].value_i ();
  patch_i_ = 0;
  if (version.size () >= 3)
    patch_i_ = version[2].value_i ();

  if (version.size () >= 4)
    extra_patch_str_ = version[3];
}

String
Lilypond_version::str () const
{
  return to_str (major_i_) + "." + to_str (minor_i_) + "." + to_str (patch_i_);
}

Lilypond_version::operator int () const
{
    // ugh
  return 100000 * major_i_ + 1000 * minor_i_ + patch_i_;
}

