/*
  text-item.hh -- part of GNU LilyPond

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
    void init (Text_def* tdef_l); 
 
public:

    /// do I have width?
    bool fat_b_;
    
    /* ***************/

    Text_item (General_script_def*,int dir=0);
    virtual ~Text_item();
    DECLARE_MY_RUNTIME_TYPEINFO;

protected:
    General_script_def * tdef_p_;

    virtual Interval symbol_height()const;

    virtual Molecule* brew_molecule_p() const;
    virtual void do_pre_processing();
};


#endif // TEXT_HH

