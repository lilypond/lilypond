/*
  bar.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH
#include "item.hh"

struct Bar: Item {
    const char * name() const;
    String type;
    
    Bar(String type);

    Molecule*brew_molecule_p()const;
};
#endif // BAR_HH

