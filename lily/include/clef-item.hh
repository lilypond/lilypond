
/*
  clef-item.hh -- declare Clef_item

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef CLEFITEM_HH
#define CLEFITEM_HH
#include "item.hh"

/**
  Set a clef in a staff.
 */
class Clef_item : public Item {
protected:
    Molecule* brew_molecule_p()const;
public:
    
    String type_;
    int y_off;

    /// is this a change clef (smaller size)?
    bool change;

    /* *************** */
NAME_MEMBERS();
    Clef_item();
    void read(Clef_register const&);
    void read(String);
};

#endif // CLEFITEM_HH


