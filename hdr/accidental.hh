/*
  accidental.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef ACCIDENTAL_HH
#define ACCIDENTAL_HH
#include "item.hh"

struct Accidental : Item {
    int type,pos; 

    void preprocess();
    Accidental(int type, int position);
    void print()const;
private:
    void brew_molecole();
};
#endif // ACCIDENTAL_HH

