/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "colhpos.hh"
#include "axis-group-spanner.hh"
#include "super-element.hh"

/// the columns of a score that form one line.
class Line_of_score : public Axis_group_spanner, public Super_element
{
public:
  Link_array<Paper_column> cols_;

  Line_of_score();
    
  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
    
  Line_of_score * set_breaking (Array<Column_x_positions> const&, int j);


  void output_all (bool last_line);
  void add_column (Paper_column*);
  
protected:
  virtual void do_substitute_element_pointer (Score_element*, Score_element*);
  virtual Link_array<Score_element> get_extra_dependencies () const;
  virtual void do_print() const;
  VIRTUAL_COPY_CONS(Score_element);
};

#endif

