// the breaking problem for a score.

#ifndef PSCORE_HH
#define PSCORE_HH


#include "vray.hh"
#include "pcol.hh"
#include "pstaff.hh"

/// all stuff which goes onto paper
struct PScore {
    Paperdef *paper_;		// indirection.
    
    /// the columns, ordered left to right
    IPointerList<PCol *> cols;

    /// the idealspacings, no particular order
    IPointerList<Idealspacing*> suz;

    /// the staffs ordered top to bottom
    IPointerList<PStaff*> staffs;

    /// all symbols in score. No particular order.
    IPointerList<Item*> its;

    /// if broken, the different lines
    IPointerList<Line_of_score*> lines;

    /// crescs etc; no particular order
    IPointerList<Spanner *> spanners;

    /// broken spanners
    IPointerList<Spanner*> broken_spans;

    /****************/

    void add_broken(Spanner*);
    
    svec<Item*> select_items(PStaff*, PCol*);

    /// before calc_breaking
    void preprocess();
    
    void calc_breaking();
    /**
      calculate where the lines are to be broken.

      POST
    
      lines contain the broken lines.
     */

    /// after calc_breaking
    void postprocess();
    
    /// search all pcols which are breakable.
    svec<const PCol *> find_breaks() const;

    /// add a line to the broken stuff. Positions given in #config#
    void add_line(svec<const PCol *> curline, svec<Real> config);

    /// helper: solve for the columns in #curline#.
    svec<Real> solve_line(svec<const PCol *> curline) const;

    void add(PStaff *);
    /// add item
    void typeset_item(Item *,  PCol *,PStaff*,int=1);

    /// add an Spanner
    void typeset_spanner(Spanner*, PStaff*);
 
    ///    add to bottom of pcols
    void add(PCol*);
    /**

    */
    void output(Tex_stream &ts);

    Idealspacing* get_spacing(PCol *, PCol *);
    /*
    get the spacing between c1 and c2, create one if necessary.
    */

    /// return argument as a cursor.
    PCursor<PCol *> find_col(const PCol *)const;

    /// delete unused columns
    void clean_cols();


    /// check if the spacing/breaking problem is well-stated
    void problem_OK() const;

    /// invarinants
    void OK()const;
    PScore(Paperdef*);
    void print() const;

    /// does curline fit on the paper?
    bool feasible(svec<const PCol *> curline) const;

    /// which is first (left, higher)
    int compare_pcols(const PCol*, const PCol*)const;
};
/** notes, signs, symbols in a score can be grouped in two ways:
    horizontally (staffwise), and vertically (columns). #PScore#
    contains the items, the columns and the staffs.
 */
#endif
