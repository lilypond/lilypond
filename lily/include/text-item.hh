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
    General_script_def* tdef_p_;

protected:
    virtual ~Text_item ();
    virtual Interval symbol_height () const;
    virtual Molecule* do_brew_molecule_p () const;
    virtual void do_pre_processing ();
    virtual Real get_position_f () const;
};


#endif // TEXT_HH

