#ifndef COLS_HH
#define COLS_HH

#include "glob.hh"
#include "boxes.hh"
#include "plist.hh"
#include "item.hh"

/// stuff grouped vertically.
struct PCol {
    PointerList<const Item*> its;
    PointerList<const Spanner*> stoppers, starters;
    

    /// prebreak is put before end of line.
    PCol *prebreak_p_;
    /**
    if broken here, then (*this) column is discarded, and prebreak
    is put at end of line, owned by Col
    */

    /// postbreak at beginning of the new line
    PCol *postbreak_p_;
    /**  \See{prebreak}
    */
    
    PCol *daddy_l_;
    /** if this column is pre or postbreak, then this field points to
     the parent.  */
    
    /// if lines are broken then this column is in #line#
    const Line_of_score *line_l_;

    /// if lines are broken then this column x-coord #hpos#
    Real hpos;

    PScore * pscore_l_;

    /****************/
    /// which  one (left =0)
    int rank() const;

    /// does this column have items, does it have spacings attached?
    bool used() const;
    
    void add(Item *i);

    /// Can this be broken? true eg. for bars. 
    bool breakable()const;
    
    Interval width() const;
    ~PCol();
    PCol(PCol * parent);

    /// which col comes first?
    static int compare(const PCol &c1, const PCol &c2);
    /**
    signed compare on columns.

    return < 0 if c1 < c2.
    */

    void OK() const;
    void set_breakable();
    void print()const;
private:
    PCol(PCol const&){}
};
/**
    This is a class to address items vertically. It contains the data for:
    \begin{itemize}
    \item
    unbroken score
    \item
    broken score
    \item
    the linespacing problem
    \end{itemize}
  */

#include "compare.hh"
instantiate_compare(PCol &, PCol::compare);
     

#endif
