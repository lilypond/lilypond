#ifndef COLS_HH
#define COLS_HH

#include "glob.hh"
#include "boxes.hh"
#include "plist.hh"
#include "item.hh"


/**
   stuff grouped vertically.
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

class PCol { 
public:
    Link_list<Item *> its;
    Link_list<Spanner *> stoppers, starters;
    
    /** prebreak is put before end of line.
    if broken here, then (*this) column is discarded, and prebreak
    is put at end of line, owned by Col
    */
    PCol *prebreak_p_;

    /// postbreak at beginning of the new line
    PCol *postbreak_p_;
    
    /** if this column is pre or postbreak, then this field points to
     the parent.  */
    PCol *daddy_l_;
    
    /// if lines are broken then this column is in #line#
    Line_of_score *line_l_;

    /** if lines are broken then this column x-coord #hpos# if not
      known, then hpos == -1.(ugh?)  */

    Real hpos;			// should use ptr?

    bool error_mark_b_;
    bool used_b_ ;		// manual override.. 
    
    PScore * pscore_l_;

    /* *************** */
    /// which  one (left =0)
    int rank_i() const;

    /// does this column have items
    bool used_b() const;
    bool breakpoint_b() const;
    void clean_breakable_items();
    
    void add(Item *i);

    /// Can this be broken? true eg. for bars. 
    bool breakable_b()const;
    
    Interval width() const;
    ~PCol();
    PCol(PCol * parent);

    /**
      which col comes first?.
      signed compare on columns.

      @return < 0 if c1 < c2.
    */
    static int compare(const PCol &c1, const PCol &c2);
    void set_rank(int);

    void OK() const;
    void set_breakable();
    void print()const;
private:
    
    /**
      The ranking: left is smaller than right 
      -1 is uninitialised.
     */
    int rank_i_;
    PCol(PCol const&){}
};


#include "compare.hh"
instantiate_compare(PCol &, PCol::compare);
     

#endif
