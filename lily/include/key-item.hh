/*
  key-item.hh -- part of GNU LilyPond

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
    bool default_b_;

    
    DECLARE_MY_RUNTIME_TYPEINFO;
    SCORE_ELEM_CLONE(Key_item);

    Key_item (int cposition);
    void add (int pitch, int acc);
    void read (const Key_engraver&);
    void set_c_position (int);
protected:
    virtual void do_pre_processing();
    Molecule* brew_molecule_p() const;
};

#endif // KEYITEM_HH
