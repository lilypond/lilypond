/*
  text.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef TEXT_ITEM_HH
#define TEXT_ITEM_HH

#include "text-def.hh"
#include "item.hh"
#include "staff-side.hh"

/**
  print a fixed width text above or below the staff.
 */
class Text_item : public Item ,public Staff_side{
    void init(Text_def* tdef_l); 
    Text_def* tdef_p_;

public:
    Text_def * tdef_l();
    int pos_i_;
        
    /* ***************/

    NAME_MEMBERS(Text_item);
    virtual void set_default_index();
    Molecule* brew_molecule_p() const;
    void do_pre_processing();
    Text_item(Text_def*);
    Text_item(Text_req*);
    ~Text_item();
};


#endif // TEXT_HH

