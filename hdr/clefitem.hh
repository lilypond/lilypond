
/*
  clefitem.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef CLEFITEM_HH
#define CLEFITEM_HH
#include "item.hh"


struct Clef_item : Item {
    String type;
    int y_off;

    Clef_item();
    void read(Clef);
    void read(String);
    void preprocess();
};

#endif // CLEFITEM_HH


