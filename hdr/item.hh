#ifndef ITEM_HH
#define ITEM_HH

#include "glob.hh"
#include "boxes.hh"
#include "string.hh"
    
/// a horizontally fixed size element of the score
struct Item {
    /// indirection to the column it is in
    PCol * pcol_;

    /// indirection to the pstaff it is in
    PStaff *pstaff_;
    
    /// member: the symbols
    Molecule *output;

    /// 
    Offset offset_;
    /**
      This is  needed, because #output# may still be
      NULL.
      */
    /****************/

    void translate(Offset);
    
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
    Paperdef *paper() const;
};
/** Item is the datastructure for printables whose width is known
  before the spacing is calculated

  NB. This doesn't mean an Item has to initialize the output field before
  spacing calculation. 
  
*/


#endif
