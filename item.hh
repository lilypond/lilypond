#ifndef ITEM_HH
#define ITEM_HH

#include "glob.hh"
#include "boxes.hh"
#include "string.hh"
#include "tex.hh"

    
/// a horizontally fixed size element of the score
struct Item {
    const PCol * col;
    Molecule *output;
    
    PStaff *pstaff_;
    /** needed for knowing at which staff to output this item
    */

    /****************/

    virtual Interval width() const;    
    virtual Interval height() const;
    String TeXstring () const ;
    Item();
    void print()const;
    virtual ~Item();
};
/** An item must be part of a Column
*/


#endif
