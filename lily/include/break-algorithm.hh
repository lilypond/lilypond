/*
  break-algorithm.hh -- declare  Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BREAK_HH
#define BREAK_HH

#include "array.hh"
#include "interval.hh"
#include "lily-proto.hh"
#include "column-x-positions.hh"


/** Class representation of an algorithm which decides where to put
  the column, and where to break lines.

  JUNKME.
  
  */
class Break_algorithm {
protected:
  Paper_score *pscore_l_;
  Real linewidth_f_;

  /// search all pcols which are breakable.
  Link_array<Grob> find_breaks () const;

  Array<int> find_break_indices () const;
    

  /// helper: solve for the columns in #curline#.
  void solve_line (Column_x_positions*) const;

  /// does curline fit on the paper?    
  bool feasible (Link_array<Grob>) const;
    

  Simple_spacer* generate_spacing_problem (Link_array<Grob>, Interval) const;

  virtual Array<Column_x_positions> do_solve () const=0;

public:
  Simple_spacer* (*get_line_spacer) ();
  Break_algorithm ();
  void set_pscore (Paper_score*);

  Array<Column_x_positions> solve () const;
};

#endif // BREAK_HH

