#ifndef ITEM_HH
#define ITEM_HH

#include "glob.hh"
#include "boxes.hh"
#include "string.hh"
#include "staffelem.hh"


/// a horizontally fixed size element of the score
struct Item : Staff_elem {
    /// indirection to the column it is in
    PCol * pcol_l_;

    /****************/
    
    Item();
    void print()const;
};
/** Item is the datastructure for printables whose width is known
  before the spacing is calculated

  NB. This doesn't mean an Item has to initialize the output field before
  spacing calculation. 
  
*/


#endif
