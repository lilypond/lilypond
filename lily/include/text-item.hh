/*
  text.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef TEXT_ITEM_HH
#define TEXT_ITEM_HH

#include "text-def.hh"
#include "item.hh"
  
struct Text_item : Item {
    int pos_i_;
    int staffsize_i_;
    int dir_i_;
    Text_def* tdef_l_;
    
    /* ***************/
    NAME_MEMBERS(Text_item);
    virtual void set_default_index();
    Molecule* brew_molecule_p() const;
    void do_pre_processing();
    
    Text_item(Text_req*,int);
};


#endif // TEXT_HH

