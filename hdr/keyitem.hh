/*
  keyitem.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "item.hh"
#include "varray.hh"


/// An item which places accidentals at the start of the line
struct Key_item : Item {
    Array<int> pitch;
    Array<int> acc;
    int c_position;

    
    /* *************** */
    const char * name() const;    
    Key_item(int cposition);
    void add(int pitch, int acc);
    void read(const Key_register&);
    void set_c_position(int);
    void preprocess();
    Molecule* brew_molecule_p()const;
};

#endif // KEYITEM_HH
