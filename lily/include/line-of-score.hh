/*
  line-of-score.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef SCORELINE_HH
#define SCORELINE_HH

#include "colhpos.hh"
#include "spanner.hh"
#include "super-element.hh"

/// the columns of a score that form one line.
class Line_of_score : public Spanner , public Super_element
{
public:
  Link_array<Paper_column> cols_;
  bool error_mark_b_;

  
  Line_of_score();
    
  void add_element (Score_element *);

  /// is #c# contained in #*this#?
  bool contains_b (Paper_column const *c) const;
    
  Link_array<Line_of_score> get_lines() const;
  Line_of_score * set_breaking (Array<Column_x_positions> const&, int j) const;

protected:
  virtual Link_array<Score_element> get_extra_dependencies () const;


  virtual Interval do_width() const;
  virtual void do_print() const;
  VIRTUAL_COPY_CONS(Score_element);
};

#endif

