// the breaking problem for a score.

#ifndef PSCORE_HH
#define PSCORE_HH


#include "vray.hh"
#include "pcol.hh"
#include "pstaff.hh"

/// all stuff which goes onto paper
struct PScore {
    /// width of paper
    Real linewidth;
    
    /// the columns, ordered left to right
    PointerList<PCol *> cols;

    /// the idealspacings, no particular order
    PointerList<Idealspacing*> suz;

    /// the staffs ordered top to bottom
    PointerList<PStaff*> staffs;

    /// all symbols in score. No particular order.
    PointerList<Item*> its;

    /// if broken, the different lines
    PointerList<Line_of_score*> lines;

    /// crescs etc; no particular order
    PointerList<Spanner *> spanners;

    /****************************************************************/

    svec<Item*> select_items(PStaff*, PCol*);
    
    void calc_breaking();
    /**
      calculate where the lines are to be broken.

      POST
    
      lines contain the broken lines.
     */

    /// search all pcols which are breakable.
    svec<const PCol *> find_breaks() const;

    /// add a line to the broken stuff. Positions given in #config#
    void add_line(svec<const PCol *> curline, svec<Real> config);

    /// helper: solve for the columns in #curline#.
    svec<Real> solve_line(svec<const PCol *> curline) const;

    void add(PStaff *);
    /// add item
    void typeset_item(Item *,  PCol *,PStaff*,int=1);
    ///    add to bottom of pcols
    void add(PCol*);
    /**

    */
    void output(Tex_stream &ts);

    Idealspacing* get_spacing(PCol *, PCol *);
    /*
    get the spacing between c1 and c2, create one if necessary.
    */


    PCursor<PCol *> find_col(PCol *);
    void clean_cols();
    void problem_OK()const ;
    void OK()const ;
    PScore();
    void print() const;
};
/** notes, signs, symbols in a score can be grouped in two ways:
    horizontally (staffwise), and vertically (columns). #PScore#
    contains the items, the columns and the staffs.
 */
#endif
