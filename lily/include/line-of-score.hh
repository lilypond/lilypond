/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "protected-scm.hh" 
#include "column-x-positions.hh"
#include "spanner.hh"

/**
   The columns of a score that form one line.  The toplevel element.
   Any element has a Line_of_score as both X and Y reference
   point. The Paper_score contains one element of this type. Control
   enters the Score_element dependency calculation from this single
   Line_of_score object.
   
   
  properties:

    all-elements -- list of all score elements in this line. Needed
      for protecting elements from GC.

    columns -- list of all paper columns

  */
class Line_of_score : public Spanner
{
public:

  
  void post_processing();

  /// -> SCM
  int rank_i_;

  Protected_scm output_;
  Line_of_score();
    
  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
  int element_count () const;

  void break_into_pieces (Array<Column_x_positions> const&);
  void output_lines ();

  Link_array<Item> broken_col_range (Item const*, Item const*) const;
  Link_array<Paper_column> column_l_arr () const;
  
  void add_column (Paper_column*);
  void typeset_element (Score_element*);
  void output_molecule (SCM, Offset);
  void output_scheme (SCM);
  void pre_processing ();
protected:
  VIRTUAL_COPY_CONS(Score_element);
};

#endif

