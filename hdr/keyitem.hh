/*
  keyitem.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef KEYITEM_HH
#define KEYITEM_HH

#include "item.hh"
#include "vray.hh"
struct Keyitem : Item {
    svec<int> pitch;
    svec<int> acc;
    int c_position;

    /****************/
    
    Keyitem(int cposition);
    void add(int pitch, int acc);
    void read(svec<int> k);
    void preprocess();

private:
    void brew_molecole();
};
#endif // KEYITEM_HH
