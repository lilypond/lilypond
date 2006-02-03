/*
  break-algorithm.hh -- declare Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BREAK_HH
#define BREAK_HH

#include "interval.hh"
#include "column-x-positions.hh"

/** Class representation of an algorithm which decides where to put
    the column, and where to break lines.

    JUNKME.
*/
class Break_algorithm
{
protected:
  Paper_score *pscore_;
  Real linewidth_;

  Link_array__Grob_ find_breaks () const;
  std::vector<int> find_break_indices () const;
  void solve_line (Column_x_positions *) const;
  bool feasible (Link_array__Grob_ const &) const;

  Simple_spacer_wrapper *generate_spacing_problem (Link_array__Grob_ const &,
						   Interval) const;
  virtual std::vector<Column_x_positions> do_solve () const = 0;

public:
  virtual ~Break_algorithm ();
  Simple_spacer *(*get_line_spacer) ();
  Break_algorithm ();
  void set_pscore (Paper_score *);
  std::vector<Column_x_positions> solve () const;
};

#endif // BREAK_HH

