#ifndef ITEM_HH
#define ITEM_HH

#include "glob.hh"
#include "boxes.hh"
#include "string.hh"
#include "tex.hh"

/// a symbol which is attached between two columns.
struct Spanner {
    const PCol *left, *right;
    Stretchable_symbol *strets;
    PStaff * pstaff_;
    ///      clone a piece of  this spanner.
    Spanner *broken_at(const PCol *c1, const PCol *c2) const; 
    /**
 
    PRE
    c1 >= start, c2  <= stop
    */
    String TeXstring () const ;
    Spanner();
};
/** Spanner should know about the items which it should consider:
    e.g. slurs should be steep enough to "enclose" all those items. This
    is absolutely necessary for beams, since they have to adjust the
    length of stems of notes they encompass.

    */
    
/// a fixed size element of the score
struct Item {
    virtual Interval width() const;    

    const PCol * col;
    Output *output;
    
    PStaff *pstaff_;
    /** needed for knowing at which staff to output this item
    */
    String TeXstring () const ;
    Item();
};
/** An item must be part of a Column
*/

#endif
