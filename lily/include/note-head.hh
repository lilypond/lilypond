/*
  note-head.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH

#include "rhythmic-head.hh"

/** ball at the end of the stem. Takes care of:

  * help lines  

  */

class Note_head : public Rhythmic_head
{
public:
  static int compare (Note_head * const &a, Note_head *const &b) ;

  Molecule ledger_line (Interval) const;
protected:
  
  virtual void do_pre_processing();
  virtual Molecule do_brew_molecule() const;
};
#endif // NOTEHEAD_HH

