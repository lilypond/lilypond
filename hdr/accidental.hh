/*
  accidental.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef ACCIDENTAL_HH
#define ACCIDENTAL_HH
#include "item.hh"

struct Accidental : Item {
const char * name() const;
    int type,pos; 

    void preprocess();
    Accidental(int type, int position);
    void do_print()constt;
private:
    void brew_molecule();
};
#endif // ACCIDENTAL_HH

