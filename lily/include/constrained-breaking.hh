/*
  constrained-breaking.hh -- declare a line breaker that
  supports limits on the number of systems

  source file of the GNU LilyPond music typesetter

  (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CONSTRAINED_BREAKING_HH
#define CONSTRAINED_BREAKING_HH

#include "break-algorithm.hh"

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
  Real force_;
  Real penalty_;
  Column_x_positions const *line_config_;

  Constrained_break_node ()
  {
    prev_ = -1;
    demerits_ = infinity_f;
    force_ = infinity_f;
    penalty_ = 0;
    line_config_ = 0;
  }

  void print () const
  {
    printf ("prev break %d, demerits %f\n",
	    prev_, demerits_);
  }
};

/*
   A dynamic programming solution to breaking scores into lines
*/
class Constrained_breaking : public Break_algorithm
{
public:
  std::vector<Column_x_positions> solve ();
  Constrained_breaking ();
  Constrained_breaking (std::vector<int> const &start_col_posns);

  std::vector<Column_x_positions> get_solution(vsize start, vsize end, vsize sys_count);
  Real get_demerits (vsize start, vsize end, vsize sys_count);
  Real get_force (vsize start, vsize end, vsize sys_count);
  Real get_penalty (vsize start, vsize end, vsize sys_count);
  int get_max_systems (vsize start, vsize end);
  int get_min_systems (vsize start, vsize end);

  /* get the page penalty of system number sys with the given breaking */
  Real get_page_penalty (vsize start, vsize end, vsize sys_count, vsize sys, bool turn);

  void resize (vsize systems);

private:
  vsize valid_systems_;
  vsize systems_;

  /* the (i,j)th entry is the column configuration for breaking between
    columns i and j */
  std::vector<Column_x_positions> cols_;
  vsize cols_rank_;

  /* the [i](j,k)th entry is the score for fitting the first k bars onto the
    first j systems, starting at the i'th allowed starting column */
  std::vector<std::vector<Constrained_break_node> > state_;

  vector<int> start_;         /* the columns at which we might be asked to start breaking */
  vector<int> starting_breakpoints_; /* the corresponding index in breaks_ */

  vector<Grob*> all_;
  std::vector<int> breaks_;

  Column_x_positions space_line (vsize start_col, vsize end_col);
  void prepare_solution (vsize start, vsize end, vsize sys_count, vsize *rank, vsize *brk);

  void combine_demerits (Column_x_positions const *, Column_x_positions const *,
                         Real *force, Real *pen, Real *dem) const;

  bool calc_subproblem(vsize start, vsize systems, vsize max_break_index);
};
#endif /* CONSTRAINED_BREAKING_HH */
