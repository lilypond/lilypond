/*
  p-score.hh -- declare PScore

  source file of the LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef P_SCORE_HH
#define P_SCORE_HH

#include "colhpos.hh"
#include "varray.hh"
#include "lily-proto.hh"
#include "p-col.hh"
#include "p-staff.hh"


/** all stuff which goes onto paper. notes, signs, symbols in a score can be grouped in two ways:
    horizontally (staffwise), and vertically (columns). #PScore#
    contains the items, the columns and the staffs.
 */

struct PScore {
    Paper_def *paper_l_;
    
    /// the columns, ordered left to right
    Pointer_list<PCol *> cols;

    /// the idealspacings, no particular order
    Pointer_list<Idealspacing*> suz;

    /// the staffs ordered top to bottom
    Pointer_list<PStaff*> staffs;

    /// all symbols in score. No particular order.
    Pointer_list<Item*> its;

    /// if broken, the different lines
    Pointer_list<Line_of_score*> lines;

    /// crescs etc; no particular order
    Pointer_list<Spanner *> spanners;

    /// broken spanners
    Pointer_list<Spanner*> broken_spans;

    Pointer_list<Vertical_spanner*> vspan_p_list_;
    /* *************** */
    /* CONSTRUCTION */
    
    PScore(Paper_def*);
    /// add a line to the broken stuff. Positions given in #config#
    void set_breaking(Array<Col_hpositions> const &);

    void add(PStaff *);
    

    /** add an item.
       add the item in specified containers. If breakstatus is set
       properly, add it to the {pre,post}break of the pcol.
       */
    void typeset_item(Item *item_p,  PCol *pcol_l,PStaff*pstaf_l,int breakstatus=1);

    /// add a Spanner
    void typeset_spanner(Spanner*, PStaff*);
 
    ///    add to bottom of pcols
    void add(PCol*);
    void add_broken(Spanner*);

    /* INSPECTION */
    Array<Item*> select_items(PStaff*, PCol*);

     /**
       @return argument as a cursor of the list
       */
    PCursor<PCol *> find_col(PCol const *)const;

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

#endif
