
/*
  extender-spanner.hh -- part of GNU LilyPond

  Copyright (C) 1998--2023 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LYRIC_EXTENDER_HH
#define LYRIC_EXTENDER_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

/*
  Extenders must be entered manually for now.

  Although it would be possible for Lily to determine where to
  put extender lines, it's quite a tricky thing to do.  Also,
  this would demand quite strict lyrics entries.

  Note: the extender is only used for one-syllable words, or
  for on a word's last syllable.  The extender should be aligned
  with the left side of the last note of the melissima, and not
  extend beond, lasting the whole duration of the melissima
*/

class Lyric_extender
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

#endif // LYRIC_EXTENDER_HH
