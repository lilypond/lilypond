/*
  localkeyitem.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef LOCALKEYITEM_HH
#define LOCALKEYITEM_HH
#include "item.hh"
#include "vray.hh"

struct Local_acc {
    int name , acc, octave;
    static int compare(Local_acc&, Local_acc&);
};

struct Local_key_item : Item {
    svec<Local_acc> accs;

    int c0_position;

    /****************/
    
    Local_key_item(int c0position);
    void add(int oct, int pitch, int acc);

    void preprocess();

private:
    void brew_molecole();

};
#endif // LOCALKEYITEM_HH

