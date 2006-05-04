/*
  constrained-breaking.hh -- declare a line breaker that
  supports limits on the number of systems

  source file of the GNU LilyPond music typesetter

  (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CONSTRAINED_BREAKING_HH
#define CONSTRAINED_BREAKING_HH

#include "break-algorithm.hh"

enum Fordfor {
  FORBID = -1,
  DEFAULT = 0,
  FORCE = 1
};

struct Line_details {
  Real force_;
  Real extent_;   /* Y-exent of the system */
  Real padding_;  /* compulsory space after this system (if we're not last on a page) */
  Real space_;    /* spring length (stretches over extent_ but not over padding_) */
  Real inverse_hooke_;

  Fordfor line_break_;
  Fordfor page_break_;
  Fordfor page_turn_;
  Real break_penalty_;
  Real page_penalty_;
  Real turn_penalty_;

  Line_details::Line_details ()
  {
    force_ = infinity_f;
    extent_ = 0;
    padding_ = 0;
    space_ = 0;
    inverse_hooke_ = 1;
    line_break_ = DEFAULT;
    page_break_ = DEFAULT;
    page_turn_ = DEFAULT;
    break_penalty_ = 0;
    page_penalty_ = 0;
    turn_penalty_ = 0;
  }
};

/*
   Helper to trace back an optimal path
*/
struct Constrained_break_node
{
  /* the number of bars in all the systems before this one
  */
  int prev_;

  /* unlike the Gourlay breaker, this is the sum of all demerits up to,
   * and including, this line */
  Real demerits_;
  struct Line_details details_;

  Constrained_break_node ()
  {
    prev_ = -1;
    demerits_ = infinity_f;
  }

  void print () const
  {
    printf ("prev break %d, demerits %f\n", prev_, demerits_);
  }
};

/*
   A dynamic programming solution to breaking scores into lines
*/
class Constrained_breaking : public Break_algorithm
{
public:
  vector<Column_x_positions> solve ();
  Constrained_breaking ();
  Constrained_breaking (vector<vsize> const &start_col_posns);

  vector<Column_x_positions> get_solution(vsize start, vsize end, vsize sys_count);
  vector<Line_details> get_details (vsize start, vsize end, vsize sys_count);
  int get_max_systems (vsize start, vsize end);
  int get_min_systems (vsize start, vsize end);

  void resize (vsize systems);

private:
  vsize valid_systems_;
  vsize systems_;

  /* the (i,j)th entry is the configuration for breaking between
    columns i and j */
  vector<Line_details> lines_;
  vsize lines_rank_;

  /* the [i](j,k)th entry is the score for fitting the first k bars onto the
    first j systems, starting at the i'th allowed starting column */
  vector<vector<Constrained_break_node> > state_;

  vector<vsize> start_;         /* the columns at which we might be asked to start breaking */
  vector<vsize> starting_breakpoints_; /* the corresponding index in breaks_ */

  vector<Grob*> all_;
  vector<vsize> breaks_;

  Column_x_positions space_line (vsize start_col, vsize end_col);
  void prepare_solution (vsize start, vsize end, vsize sys_count, vsize *rank, vsize *brk);

  Real combine_demerits (Real force, Real prev_force);

  bool calc_subproblem(vsize start, vsize systems, vsize max_break_index);
};
#endif /* CONSTRAINED_BREAKING_HH */
