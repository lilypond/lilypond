/*
  bar.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH
#include "item.hh"
/**
  TODO: connections with pre and postbreak
 */
class Bar:public Item {
public:
    String type;
    
    NAME_MEMBERS();
    Bar(String type);
private:
    void do_print() const;
    Molecule*brew_molecule_p()const;
};
#endif // BAR_HH

