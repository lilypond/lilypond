/*
  lilypond-version.hh -- declare Lilypond_version

  source file of the GNU LilyPond music typesetter

  (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#ifndef LILYPOND_VERSION_HH
#define LILYPOND_VERSION_HH

#include "string.hh"

struct Lilypond_version 
{
  Lilypond_version (int major, int minor, int patch);
  Lilypond_version (String str);

  String str () const;
  operator int () const;

  int major_i_;
  int minor_i_;
  int patch_i_;
  String extra_patch_str_;
};

extern Lilypond_version oldest_version;

#endif // LILYPOND_VERSION_HH
