/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "column-x-positions.hh"
#include "axis-group-spanner.hh"
#include "super-element.hh"

/// the columns of a score that form one line.
class Line_of_score : public Axis_group_spanner, public Super_element
{
public:
  int rank_i_;
  
  Line_of_score();
    
  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
    
  static int compare (Line_of_score* const &,Line_of_score* const &);

  void break_into_pieces (Array<Column_x_positions> const&);
  void output_lines ();
  void output_line (bool last_line);
  void add_column (Paper_column*);
  
protected:
  virtual void do_print() const;
  VIRTUAL_COPY_CONS(Score_element);
};

#endif

