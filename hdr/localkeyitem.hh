/*
  localkeyitem.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef LOCALKEYITEM_HH
#define LOCALKEYITEM_HH
#include "item.hh"
#include "varray.hh"

struct Local_acc {
    int name , acc, octave;
    static int compare(Local_acc&, Local_acc&);
};

struct Local_key_item : Item {
    Array<Local_acc> accs;
    Array<Notehead*> group;
    int c0_position;		// move into walker

    /****************/
    
    Local_key_item(int c0position);
    void add(int oct, int pitch, int acc, Notehead*);

    void do_pre_processing();    
    Molecule* brew_molecule()const;
};
#endif // LOCALKEYITEM_HH

