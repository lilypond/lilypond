/*
  text.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef TEXT_ITEM_HH
#define TEXT_ITEM_HH

#include "text-def.hh"
#include "item.hh"

/**
  print a fixed width text above or below the staff.
 */
class Text_item : public Item {
    void init(Text_def* tdef_l,int staffsize_i); 
    Text_def* tdef_p_;
public:
    Text_def * tdef_l();
    int pos_i_;
    int staffsize_i_;
    int dir_i_;
        
    /* ***************/

    NAME_MEMBERS(Text_item);
    virtual void set_default_index();
    Molecule* brew_molecule_p() const;
    void do_pre_processing();
    Text_item(Text_def*,int);
    Text_item(Text_req*,int);
    ~Text_item();
};


#endif // TEXT_HH

