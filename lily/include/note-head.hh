/*
  note-head.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH

#include "rhythmic-head.hh"

/** ball at the end of the stem. Takes care of:

  * help lines  

  Properties

  style -- symbol that sets note head style

  */

class Note_head : public Rhythmic_head
{
public:
  static int compare (Note_head * const &a, Note_head *const &b) ;
 static SCM scheme_molecule (SCM);
  

  Molecule ledger_line (Interval) const;
  Note_head (SCM);
protected:
  virtual void before_line_breaking ();
  virtual Molecule do_brew_molecule() const;
};
#endif // NOTEHEAD_HH

