/*
  local-key-item.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef LOCALKEYITEM_HH
#define LOCALKEYITEM_HH
#include "item.hh"
#include "varray.hh"
#include "musical-pitch.hh"

/**
  Accidentals which can be different for each octave.

  TODO:
  update item if Items are removed

  TODO
  
  figure out private/public
  
 */
class Local_key_item : public Item {
public:
    DECLARE_MY_RUNTIME_TYPEINFO;
    Array<Musical_pitch> accs;
    Link_array<Item> support_items_;
    int c0_position;


    
    Local_key_item (int c0position);
    void add_support (Item*);
    void add (Musical_pitch);
protected:
    virtual void do_pre_processing();    
    virtual void do_substitute_dependency (Score_elem*,Score_elem*);
    virtual Molecule* brew_molecule_p() const;
};
#endif // LOCALKEYITEM_HH

