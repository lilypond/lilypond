
/*
  clefitem.hh -- declare Clef_item

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef CLEFITEM_HH
#define CLEFITEM_HH
#include "item.hh"


struct Clef_item : Item {
    String type;
    int y_off;

    /// is this a change clef (smaller size)?
    bool change;


    /* *************** */
    const char * name() const;
    Clef_item();
    void read(Clef_register const&);
    void read(String);
    Molecule* brew_molecule_p()const;
};

#endif // CLEFITEM_HH


