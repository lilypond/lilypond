
/*
  clefitem.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef CLEFITEM_HH
#define CLEFITEM_HH
#include "item.hh"


struct Clef_item : Item {
    String type;
    int y_off;
    bool change;


    /****************/

    Clef_item();
    void read(Clef);
    void read(String);
    Molecule* brew_molecule_p()const;
};

#endif // CLEFITEM_HH


