/*
  break.hh -- declare  Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef BREAK_HH
#define BREAK_HH
#include "varray.hh"
#include "lily-proto.hh"
#include "colhpos.hh"

/** Class representation of an algorithm which decides where to put
  the column, and where to break lines.
  
  TODO: a decent algorithm, based on dynamic programming or something
  a like. A "parindent", caching of breakpoints
  
  */
class Break_algorithm {
    void generate_spacing_problem(Line_of_cols,Spacing_problem&)const;
protected:
    PScore &pscore_;
    Real linelength;

    /// search all pcols which are breakable.
    Line_of_cols find_breaks() const;

     /// helper: solve for the columns in #curline#.
    Col_hpositions solve_line(Line_of_cols) const;

    /// does curline fit on the paper?    
    bool feasible(Line_of_cols)const;
    

    /** generate a solution with no regard to idealspacings or
      constraints.  should always work */
    Col_hpositions stupid_solution(Line_of_cols) const;
    virtual Array<Col_hpositions> do_solve()const=0;
public:
    Break_algorithm(PScore&);

    /// check if the spacing/breaking problem is well-stated
    void problem_OK()const;

    Array<Col_hpositions> solve()const;
};

/// wordwrap type algorithm: move to next line if current is optimal.
struct Word_wrap : Break_algorithm {
    virtual Array<Col_hpositions> do_solve()const;
    Word_wrap(PScore&);
};
#endif // BREAK_HH

