/*
  break.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef BREAK_HH
#define BREAK_HH
#include "varray.hh"
#include "proto.hh"
#include "colhpos.hh"

struct Break_algorithm {
    PScore &pscore_;
    Real linelength;

    /****************/

    Break_algorithm(PScore&);

    /// check if the spacing/breaking problem is well-stated
    void problem_OK()const;

    /// search all pcols which are breakable.
    Line_of_cols find_breaks() const;

     /// helper: solve for the columns in #curline#.
    Col_hpositions solve_line(Line_of_cols) const;

    /// does curline fit on the paper?    
    bool feasible(Line_of_cols)const;
    
    virtual Array<Col_hpositions> solve()=0;
};

/// wordwrap type algorithm: move to next line if current is optimal.
struct Word_wrap : Break_algorithm {
    virtual Array<Col_hpositions> solve();
    Word_wrap(PScore&);
};
#endif // BREAK_HH

