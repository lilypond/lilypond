/*
  bar.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH
#include "item.hh"

struct Bar: Item {
    String type;
    
    Bar(String type);
    const char * name() const;
    void do_print() const;
    Molecule*brew_molecule_p()const;
};
#endif // BAR_HH

