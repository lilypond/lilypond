/*
  p-score.hh -- declare Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef P_SCORE_HH
#define P_SCORE_HH

#include "colhpos.hh"
#include "parray.hh"
#include "lily-proto.hh"
#include "plist.hh"
#include "music-output.hh"

/** all stuff which goes onto paper. notes, signs, symbols in a score
     #Paper_score# contains the items, the columns.
    
    */

class Paper_score : public Music_output
{
public:
  Paper_def *paper_l_;

  /// the columns, ordered left to right
  Link_list<Paper_column *> col_p_list_;

  /// crescs etc; no particular order
  Pointer_list<Spanner *> span_p_list_;

  /// other elements
  Pointer_list<Score_element*> elem_p_list_;
  
  Paper_outputter *outputter_l_;  
  Line_of_score * line_l_;
  
  Paper_score ();


  /// add to bottom of pcols
  void add_column (Paper_column*);

  /**
    @return argument as a cursor of the list
    */
  PCursor<Paper_column *> find_col (Paper_column const *) const;

  Link_array<Paper_column> col_range (Paper_column *left_l, Paper_column *right_l) const;
  Link_array<Paper_column> breakable_col_range (Paper_column*,Paper_column*) const;
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
// can't instantiate template with cygnus' gcc...
//  virtual ~Paper_score();

private:

  void remove_line (Line_of_score*);
  /// before calc_breaking
  void preprocess();

  void calc_idealspacing();
  /// calculate where the lines are to be broken, and use results
  Array<Column_x_positions> calc_breaking();

  /// after calc_breaking
  void postprocess();
    
  /// delete unused columns
  void clean_cols();
};

#endif
