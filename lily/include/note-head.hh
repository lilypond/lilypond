/*
  note-head.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
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
    
  /// -1 = lowest, 0 = inside, 1 = top
  int extremal_i_;
    
  /// needed for the help-lines
  int staff_size_i_;
  Direction x_dir_;
    
  /**
    position of top line (5 linestaff: 8)
    */
  Note_head ();
  static int compare (Note_head * const &a, Note_head *const &b) ;

protected:
  virtual Interval do_width () const;
  virtual void do_pre_processing();
  virtual Molecule* brew_molecule_p() const;
};
#endif // NOTEHEAD_HH

