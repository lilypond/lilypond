/*
  break-algorithm.hh -- declare  Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BREAK_HH
#define BREAK_HH

#include "array.hh"
#include "interval.hh"
#include "lily-proto.hh"
#include "column-x-positions.hh"


/** Class representation of an algorithm which decides where to put
  the column, and where to break lines.
  
  */
class Break_algorithm {
protected:
  Paper_score *pscore_l_;
  Real linewidth_f_;

  /// search all pcols which are breakable.
  Link_array<Paper_column> find_breaks() const;

  Array<int> find_break_indices() const;
    

  /// helper: solve for the columns in #curline#.
  void solve_line (Column_x_positions*) const;

  /// helper: approximate the energyv
  void approximate_solve_line (Column_x_positions*) const;

  /// does curline fit on the paper?    
  bool feasible (Link_array<Paper_column>) const;
    

  Simple_spacer* generate_spacing_problem (Link_array<Paper_column>, Interval) const;

  virtual Array<Column_x_positions> do_solve() const=0;
  virtual void do_set_pscore();

public:
  Simple_spacer* (*get_line_spacer)();
  Break_algorithm();
  void set_pscore (Paper_score*);

  /// check if the spacing/breaking problem is well-stated
  void problem_OK() const;
  void OK() const;
  Array<Column_x_positions> solve() const;
};

#endif // BREAK_HH

