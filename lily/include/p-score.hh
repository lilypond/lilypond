/*
  p-score.hh -- declare Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef P_SCORE_HH
#define P_SCORE_HH

#include "colhpos.hh"
#include "parray.hh"
#include "lily-proto.hh"
#include "plist.hh"

/** all stuff which goes onto paper. notes, signs, symbols in a score
     #Paper_score# contains the items, the columns.
    
    */

class Paper_score {
public:
    Paper_def *paper_l_;

    /// the columns, ordered left to right
    Pointer_list<PCol *> col_p_list_;

    /// the idealspacings, no particular order
    Pointer_list<Idealspacing*> suz_p_list_;

    /// crescs etc; no particular order
    Pointer_list<Spanner *> span_p_list_;

    /// other elements
    Pointer_list<Score_elem*> elem_p_list_;
    
    Super_elem *super_elem_l_;

    /* *************** */
    /* CONSTRUCTION */
    
    Paper_score (Paper_def*);
    /// add a line to the broken stuff. Positions given in #config#
    void set_breaking (Array<Col_hpositions> const &);

    /** add an item.
       add the item in specified containers. If breakstatus is set
       properly, add it to the {pre,post}break of the pcol.
       */
    void typeset_item (Item *item_p,  PCol *pcol_l);

    /// add to bottom of pcols
    void add (PCol*);

    /**
      @return argument as a cursor of the list
      */
    PCursor<PCol *> find_col (PCol const *)const;

    Link_array<PCol> col_range (PCol *left_l, PCol *right_l) const;
    Link_array<PCol> breakable_col_range (PCol*,PCol*) const;
    Link_array<PCol> broken_col_range (PCol*,PCol*) const;
    
    /* MAIN ROUTINES */
    void process();

    /// last deed of this struct
    void output (Tex_stream &ts);

    /* UTILITY ROUTINES */

    
    /* STANDARD ROUTINES */
    void OK()const;
    void print() const;
    ~Paper_score();
    void typeset_element (Score_elem*);
    void typeset_broken_spanner (Spanner*);
    /// add a Spanner
    void typeset_unbroken_spanner (Spanner*);
 
    
private:
    /// before calc_breaking
    void preprocess();

    void calc_idealspacing();
    /// calculate where the lines are to be broken, and use results
    void calc_breaking();

    /// after calc_breaking
    void postprocess();
    
    /// delete unused columns
    void clean_cols();
};

#endif
