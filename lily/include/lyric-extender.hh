
/*
  extender-spanner.hh -- part of GNU LilyPond

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef EXTENDER_SPANNER_HH
#define EXTENDER_SPANNER_HH

#include "spanner.hh"

/** 
  simple extender line 

  The extender is a simple line at the baseline of the lyric
  that helps show the length of a melissima (tied/slurred note).

  Extenders must be entered manually for now.

  Although it would be possible for Lily to determine where to
  put extender lines, it's quite a tricky thing to do.  Also,
  this would demand quite strict lyrics entries.

  Note: the extender is only used for one-syllable words, or
  for on a word's last syllable.  The extender should be aligned
  with the left side of the last note of the melissima, and not
  extend beond, lasting the whole duration of the melissima
  (as in MUP, urg).
  */
class Lyric_extender // interface
{
public:
  Spanner*elt_l_;
  Lyric_extender (Spanner*);
  void set_textitem (Direction, Score_element*);
  static SCM brew_molecule (SCM);
};

#endif // EXTENDER_SPANNER_HH

