#ifndef ITEM_HH
#define ITEM_HH

#include "glob.hh"
#include "boxes.hh"
#include "string.hh"
#include "staff-elem.hh"

/**
 a horizontally fixed size element of the score

  Item is the datastructure for printables whose width is known
  before the spacing is calculated

  NB. This doesn't mean an Item has to initialize the output field before
  spacing calculation. 
  
*/
struct Item : Staff_elem {
    /// indirection to the column it is in
    PCol * pcol_l_;

    /* *************** */
    virtual Item *item() { return this; }
    Item();
    void do_print()const;

    NAME_MEMBERS(Item);
};


#endif
