
/*
  lyric-extender.hh -- part of GNU LilyPond

  Copyright (C) 1998--2025 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef LYRIC_EXTENDER_HH
#define LYRIC_EXTENDER_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

/*
  Extenders may be entered manually using __, or they can be
  auto-generated when autoExtenders = ##t.

  In autoExtenders mode, any Lyric syllable gets an extender
  if the associated voice is inside a melisma and the syllable
  is not followed by a hyphen.  This causes too many extenders
  to be created, which then have to be reduced by a minimum
  lengh requirement.

  Note: the extender is only used for one-syllable words, or
  for on a word's last syllable.  The extender should be aligned
  with the left side of the last note of the melisma, and not
  extend beond, lasting the whole duration of the melisma.
*/

class Lyric_extender
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

#endif // LYRIC_EXTENDER_HH
