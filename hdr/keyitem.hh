/*
  keyitem.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "item.hh"
#include "vray.hh"

/// 
struct Keyitem : Item {
    svec<int> pitch;
    svec<int> acc;
    int c_position;

    
    /****************/
    
    Keyitem(int cposition);
    void add(int pitch, int acc);
    void read(svec<int> k);

    void preprocess();

    Molecule* brew_molecule()const;
};
/**
  An item which places accidentals at the start of the line
  */

#endif // KEYITEM_HH
