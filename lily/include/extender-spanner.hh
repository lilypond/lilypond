/*
  extender-spanner.hh -- part of GNU LilyPond

  (c)  1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef EXTENDER_SPANNER_HH
#define EXTENDER_SPANNER_HH

#include "directional-spanner.hh"

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

class Extender_spanner : public Directional_spanner
{
public:
  Extender_spanner ();
  virtual ~Extender_spanner ();

  Offset center () const;  
  void set_textitem (Direction, Text_item*);

  Drul_array<Text_item *> textitem_l_drul_;
 
protected:
  virtual Molecule* brew_molecule_p () const;
  void do_add_processing ();
  Interval do_height () const;
  void do_substitute_dependency (Score_element* o, Score_element* n);
  void do_post_processing ();
 
  VIRTUAL_COPY_CONS (Score_element);

  Extender_spanner (Extender_spanner const&);

  Drul_array<Real> dy_f_drul_;
  Drul_array<Real> dx_f_drul_;
};

#endif // EXTENDER_SPANNER_HH

