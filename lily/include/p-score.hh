/*
  p-score.hh -- declare Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef P_SCORE_HH
#define P_SCORE_HH

#include "colhpos.hh"
#include "parray.hh"
#include "lily-proto.hh"
#include "music-output.hh"
#include "lily-guile.hh"

/** all stuff which goes onto paper. notes, signs, symbols in a score
     #Paper_score# contains the items, the columns.
    
    */

class Paper_score : public Music_output
{
  /// crescs etc; no particular order
  Link_array<Spanner> span_p_arr_;

  /// other elements
  Link_array<Score_element> elem_p_arr_;
  Link_array<Score_element> break_helpers_arr_;

  SCM protected_scms_;
public:
  Paper_def *paper_l_;

  /// the columns, ordered left to right
  Link_array<Paper_column> col_l_arr_;

  Paper_outputter *outputter_l_;  
  Line_of_score * line_l_;
  
  Paper_score ();


  /// add to bottom of pcols
  void add_column (Paper_column*);

  /**
    @return index of argument.
    */
  int find_col_idx (Paper_column const *) const;

  Link_array<Item> broken_col_range (Item const*,Item const*) const;
    
    
  /* STANDARD ROUTINES */
  void print() const;
  
  void typeset_element (Score_element*);
  void typeset_broken_spanner (Spanner*);
  /// add a Spanner
  void typeset_unbroken_spanner (Spanner*);
  
    
  virtual ~Paper_score();
protected:
    /* MAIN ROUTINES */
  virtual void process();

private:
  /// before calc_breaking
  void preprocess();

  void calc_idealspacing();
  /// calculate where the lines are to be broken, and use results
  Array<Column_x_positions> calc_breaking();

  /// after calc_breaking
  void postprocess();
  Paper_score (Paper_score const &);
};

#endif
