// the breaking problem for a score.

#ifndef PSCORE_HH
#define PSCORE_HH

#include "break.hh"
#include "varray.hh"
#include "pcol.hh"
#include "pstaff.hh"

/// all stuff which goes onto paper
struct PScore {
    Paperdef *paper_l_;
    
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
    /* CONSTRUCTION */
    
    PScore(Paperdef*);
    /// add a line to the broken stuff. Positions given in #config#
    void set_breaking(Array<Col_configuration>);

    void add(PStaff *);
    
    /// add item
    void typeset_item(Item *,  PCol *,PStaff*,int=1);

    /// add a Spanner
    void typeset_spanner(Spanner*, PStaff*);
 
    ///    add to bottom of pcols
    void add(PCol*);
    void add_broken(Spanner*);

    /* INSPECTION */
    Array<Item*> select_items(PStaff*, PCol*);

    /// return argument as a cursor.
    PCursor<PCol *> find_col(const PCol *)const;

    /* MAIN ROUTINES */
    void process();

    /// last deed of this struct
    void output(Tex_stream &ts);

    /* UTILITY ROUTINES */

    /// get the spacing between c1 and c2, create one if necessary.
    Idealspacing* get_spacing(PCol *c1, PCol *c2);

    /// connect c1 and c2
    void do_connect(PCol *c1, PCol *c2, Real distance_f, Real strength_f);

    /// connect c1 and c2 and any children of c1 and c2
    void connect(PCol* c1, PCol *c2, Real distance_f,Real  strength_f= 1.0);
    
    /* STANDARD ROUTINES */
    void OK()const;
    void print() const;
private:
    /// before calc_breaking
    void preprocess();

    /// calculate where the lines are to be broken, and use results
    void calc_breaking();

    /// after calc_breaking
    void postprocess();
    
    /// delete unused columns
    void clean_cols();
};
/** notes, signs, symbols in a score can be grouped in two ways:
    horizontally (staffwise), and vertically (columns). #PScore#
    contains the items, the columns and the staffs.
 */
#endif
