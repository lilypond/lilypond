/*
  text-item.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef TEXT_ITEM_HH
#define TEXT_ITEM_HH

#include "text-def.hh"
#include "item.hh"
#include "staff-side.hh"

/**
  print a fixed width text above or below the staff.
 */
class Text_item : public Item ,public Staff_side
{
public:

    /// do I have width?
    bool fat_b_;
    

    Text_item (General_script_def* ,Direction dir=CENTER);
    virtual ~Text_item ();
    DECLARE_MY_RUNTIME_TYPEINFO;

//protected:
    // ugh: so, are we a text-def, or can this vary?
    General_script_def* tdef_p_;
protected:

    virtual Interval symbol_height () const;

    virtual Molecule* brew_molecule_p () const;
    virtual void do_pre_processing ();
    virtual Real get_position_f () const;

private:
//    void init (Text_def* tdef_l); 
};


#endif // TEXT_HH

