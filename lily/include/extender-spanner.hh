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
class Extender_spanner : public Spanner
{
public:
  Extender_spanner (SCM);
  void set_textitem (Direction, Item*);

protected:
  virtual Molecule do_brew_molecule () const;
  void after_line_breaking ();
 
  VIRTUAL_COPY_CONS (Score_element);

  Drul_array<Real> dx_f_drul_;
};

#endif // EXTENDER_SPANNER_HH

