/*
  constrained-breaking.hh -- declare a line breaker that
  supports limits on the number of systems

  source file of the GNU LilyPond music typesetter

  (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CONSTRAINED_BREAKING_HH
#define CONSTRAINED_BREAKING_HH

#include "break-algorithm.hh"

/**
   Helper to trace back an optimal path
*/
struct Constrained_break_node
{
  /** the number of bars in all the systems before this one
   */
  int prev_;

  /** unlike the Gourlay breaker, this is the sum of all demerits up to,
   * and including, this line */
  Real demerits_;
  Real force_;
  Real penalty_;
  Column_x_positions line_config_;

  Constrained_break_node ()
  {
    prev_ = -1;
    demerits_ = infinity_f;
    force_ = infinity_f;
    penalty_ = 0;
    line_config_.satisfies_constraints_ = false;
  }

  void print () const
  {
    printf ("prev break %d, demerits %f\n",
	    prev_, demerits_);
  }
};

/**
   A dynamic programming solution to breaking scores into lines
*/
class Constrained_breaking : public Break_algorithm
{
public:
  std::vector<Column_x_positions> solve ();
  Constrained_breaking ();
  Constrained_breaking (std::vector<int> const &start_col_posns);

  std::vector<Column_x_positions> get_solution(int start, int end, int sys_count);
  Real get_demerits (int start, int end, int sys_count);
  Real get_force (int start, int end, int sys_count);
  Real get_penalty (int start, int end, int sys_count);
  int get_max_systems (int start, int end);
  int get_min_systems (int start, int end);

  /* get the page penalty of system number sys with the given breaking */
  Real get_page_penalty (int start, int end, int sys_count, int sys);

  int systems_;
private:
  int valid_systems_;

  /* the (i,j)th entry is the column configuration for breaking
  between columns i and j */
  std::vector<Column_x_positions> cols_;
  int cols_rank_;

  /* the [i](j,k)th entry is the score for fitting the first k bars onto the
   first j systems, starting at the i'th allowed starting column */
  std::vector<std::vector<Constrained_break_node> > state_;

  vector<int> start_;         /* the columns at which we might be asked to start breaking */
  vector<int> starting_breakpoints_; /* the corresponding index in breaks_ */

  vector<Grob*> all_;
  std::vector<int> breaks_;

  void prepare_solution (vsize start, int end, int sys_count, int *rank, int *brk);

  void combine_demerits (Column_x_positions const &, Column_x_positions const &,
			 Real *force, Real *pen, Real *dem) const;

  bool calc_subproblem(int start, int systems, int max_break_index);
  void resize ();
};
#endif /* CONSTRAINED_BREAKING_HH */
