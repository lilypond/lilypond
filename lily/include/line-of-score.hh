/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "protected-scm.hh" 
#include "column-x-positions.hh"
#include "spanner.hh"

class Line_of_score : public Spanner
{
public:
  int rank_i_;
  void post_processing(bool);

  Line_of_score(SCM);
  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
  int element_count () const;

  void break_into_pieces (Array<Column_x_positions> const&);
  void output_lines ();

  Link_array<Item> broken_col_range (Item const*, Item const*) const;
  Link_array<Score_element> column_l_arr () const;
  
  void add_column (Paper_column*);
  void typeset_element (Score_element*);
  void output_molecule (SCM, Offset);
  void output_scheme (SCM);
  void pre_processing ();
protected:
  VIRTUAL_COPY_CONS(Score_element);
};

#endif

