#ifndef COLS_HH
#define COLS_HH

#include "glob.hh"
#include "boxes.hh"
#include "list.hh"
#include "item.hh"

/// stuff grouped vertically.
struct PCol {
    List<const Item*> its;
    List<const Spanner*> stoppers, starters;
    
    /// Can this be broken? true eg. for bars. 
    bool breakable;

    /// does this column have items, does it have spacings attached?
    bool used;

    /// prebreak is put before end of line.
    PCol *prebreak;
    /**
    if broken here, then (*this) column is discarded, and prebreak
    is put at end of line, owned by Col
    */

    /// postbreak at beginning of the new line
    PCol *postbreak;
    /**  \See{prebreak}
    */
    PCol *daddy;
    /*
    if this column is pre or postbreak, then this field points to the parent.
    */
    /// if lines are broken then this column is in #line#
    const Line_of_score *line;

    /// if lines are broken then this column x-coord #hpos#
    Real hpos;


    /****************************************************************/

    void add(const Item*i);

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
instantiate_compare(const PCol &, PCol::compare);
     

/// ideal spacing between two columns
struct Idealspacing {

    /// the ideal distance
    Real space;

    /// Hooke's constant: how strong are the "springs" attached to columns
    Real hooke;

    /// the two columns
    const PCol *left, *right;
    
    void print()const;
    void OK() const ;
    Idealspacing(const PCol *left,const PCol *right);    
};

#endif
