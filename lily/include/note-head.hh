/*
  note-head.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH

#include "rhythmic-head.hh"

/** ball at the end of the stem. Takes care of:

  * help lines  

  */

class Note_head : public Rhythmic_head {
public:
  int position_i_;

  Note_head ();
  void flip_around_stem (Direction);
  static int compare (Note_head * const &a, Note_head *const &b) ;
protected:
  virtual Interval do_width () const;
  virtual void do_pre_processing();
  virtual Molecule* do_brew_molecule_p() const;
};
#endif // NOTEHEAD_HH

