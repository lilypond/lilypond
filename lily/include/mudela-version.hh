/*
  mudela-version.hh -- declare Mudela_version

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>

*/

#ifndef MUDELA_VERSION_HH
#define MUDELA_VERSION_HH

#include "string.hh"

struct Mudela_version 
{
  Mudela_version (int major, int minor, int patch);
  Mudela_version (String str);

  String str () const;
  operator int () const;

  int major_i_;
  int minor_i_;
  int patch_i_;
};

#endif // MUDELA_VERSION_HH
