/*
  notehead.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH
#include "item.hh"

/**
 ball at the end of the stem
takes care of:

  * help lines  
  * proper placing of dots 

  */

struct Notehead : Item {
    const char * name() const;

    int position;
    /// -1 = lowest, 0 = inside, 1 = top
    int extremal;
    /// needed for the help-lines
    int staff_size;
    int dots;
    int balltype;
    int x_dir;
    
    /* *************** */
    
    void set_rhythmic(Rhythmic_req *);

    /**
      position of top line (5 linestaff: 8)
      */
    Notehead(int staff_size);
    void do_print()const;
    static int compare(Notehead*&a, Notehead*&b) ;
    Molecule* brew_molecule_p()const;
};
#endif // NOTEHEAD_HH

