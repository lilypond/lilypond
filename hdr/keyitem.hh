/*
  keyitem.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "item.hh"
#include "varray.hh"

struct Clef;

/// An item which places accidentals at the start of the line
struct Keyitem : Item {
    const char * name() const;
    Array<int> pitch;
    Array<int> acc;
    int c_position;

    
    /* *************** */
    
    Keyitem(int cposition);
    void add(int pitch, int acc);
    void read(Array<int> k);
    void read(const Clef& c);

    void preprocess();

    Molecule* brew_molecule_p()const;
};

#endif // KEYITEM_HH
