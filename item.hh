#ifndef ITEM_HH
#define ITEM_HH

#include "glob.hh"
#include "boxes.hh"
#include "string.hh"
    
/// a horizontally fixed size element of the score
struct Item {
    const PCol * col;
    Molecule *output;
    
    PStaff *pstaff_;
    /** needed for knowing at which staff to output this item
    */

    /****************/
    
    /// do calculations after determining horizontal spacing
    virtual void postprocess();

    /// do calculations before determining horizontal spacing
    virtual void preprocess();
    /**
      This is executed directly after the item is added to the
      PScore
      */
    
    virtual Interval width() const;    
    virtual Interval height() const;
    String TeXstring () const ;
    Item();
    void print()const;
    virtual ~Item();
};
/** Item is the datastructure for printables whose width is known
  before the spacing is calculated

  NB. This doesn't mean an Item has to initialize the output field before
  spacing calculation. 
  
*/


#endif
