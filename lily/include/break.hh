/*
  break.hh -- declare  Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BREAK_HH
#define BREAK_HH
#include "array.hh"
#include "interval.hh"
#include "lily-proto.hh"
#include "column-x-positions.hh"

/**
  Statistics for the number of columns calced.
 */
struct Col_stats
{
  int count_i_;
  int cols_i_;

  Col_stats(); 
  void add (Line_of_cols const&l);
  String str() const;
};

/** Class representation of an algorithm which decides where to put
  the column, and where to break lines.
  
  TODO:  A "parindent", caching of breakpoints
  
  */
class Break_algorithm {
protected:

  Paper_score *pscore_l_;
  Real linelength;

  /// search all pcols which are breakable.
  Line_of_cols find_breaks() const;

  Array<int> find_break_indices() const;
    

  /// helper: solve for the columns in #curline#.
  void solve_line (Column_x_positions*) const;

  /// helper: approximate the energyv
  void approximate_solve_line (Column_x_positions*) const;

  /// does curline fit on the paper?    
  bool feasible (Line_of_cols) const;
    

  Line_spacer* generate_spacing_problem (Line_of_cols, Interval) const;

  virtual Array<Column_x_positions> do_solve() const=0;
  virtual void do_set_pscore();

public:
  Col_stats approx_stats_;
  Col_stats exact_stats_;
  
  Line_spacer* (*get_line_spacer)();
    
  Break_algorithm();
  void set_pscore (Paper_score*);

  /// check if the spacing/breaking problem is well-stated
  void problem_OK() const;
  void OK() const;
  Array<Column_x_positions> solve() const;
};

#endif // BREAK_HH

