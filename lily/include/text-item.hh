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

    Text_item(Text_def*);
    Text_item(Text_req*);
    ~Text_item();
    NAME_MEMBERS(Text_item);
protected:
    virtual void set_default_index();
    Molecule* brew_molecule_p() const;
    virtual void do_post_processing();
};


#endif // TEXT_HH

