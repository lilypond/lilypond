/*
  notehead.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef NOTEHEAD_HH
#define NOTEHEAD_HH
#include "item.hh"

/// ball at the end of the stem
struct Notehead : public Item
{
    int position;
    /// -1 = lowest, 0 = inside, 1 = top
    int extremal;
    /// needed for the help-lines
    int staff_size;
    int dots;
    int balltype;
    int x_dir;
    
    /****************/
    

    Notehead(int staff_size);
    /**
      position of top line (5 linestaff: 8)
      */

    void print()const;
    static int compare(Notehead*&a, Notehead*&b) ;
    Molecule* brew_molecule()const;
};
/**
  takes care of:

  * help lines  
  * proper placing of dots 

  */
#endif // NOTEHEAD_HH

