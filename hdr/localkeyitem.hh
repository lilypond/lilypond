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
    const char * name() const;
    Array<Local_acc> accs;
    Array<Item*> support_items_;
    int c0_position;

    /* *************** */
    
    Local_key_item(int c0position);
    void add(Item*);
    void add(int oct, int pitch, int acc);
    void add(Melodic_req*);
    void do_pre_processing();    
    Molecule* brew_molecule_p()const;
};
#endif // LOCALKEYITEM_HH

