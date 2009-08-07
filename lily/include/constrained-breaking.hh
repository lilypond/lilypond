/*
  constrained-breaking.hh -- declare a line breaker that
  supports limits on the number of systems

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Joe Neeman <joeneeman@gmail.com>
*/

#ifndef CONSTRAINED_BREAKING_HH
#define CONSTRAINED_BREAKING_HH

#include "lily-guile.hh"
#include "matrix.hh"
#include "prob.hh"

struct Line_details {
  Grob *last_column_;
  Real force_;
  Interval extent_;   /* Y-extent of the system */

  Real padding_;  /* compulsory space after this system (if we're not
		     last on a page) */
  Real bottom_padding_;
  Real space_;    /* spring length */
  Real inverse_hooke_;

  SCM break_permission_;
  SCM page_permission_;
  SCM turn_permission_;
  Real break_penalty_;
  Real page_penalty_;
  Real turn_penalty_;

  bool title_;

  /* The page-breaker deals with forbidden page breaks by "compressing"
     two Line_detailses into one. The following fields are used by the
     page-breaker to keep track of this. If the number of fields needed
     by the page-breaker grows, it might be a good idea to create a separate
     class. */
  int compressed_lines_count_;
  int compressed_nontitle_lines_count_;

  Line_details ()
  {
    last_column_ = 0;
    force_ = infinity_f;
    padding_ = 0;
    bottom_padding_ = 0;
    space_ = 0;
    inverse_hooke_ = 1;
    break_permission_ = ly_symbol2scm ("allow");
    page_permission_ = ly_symbol2scm ("allow");
    turn_permission_ = ly_symbol2scm ("allow");
    break_penalty_ = 0;
    page_penalty_ = 0;
    turn_penalty_ = 0;
    title_ = false;
    compressed_lines_count_ = 1;
    compressed_nontitle_lines_count_ = 1;
  }

  Line_details (Prob *pb)
  {
    last_column_ = 0;
    force_ = 0;
    extent_ = unsmob_stencil (pb->get_property ("stencil")) ->extent (Y_AXIS);
    padding_ = robust_scm2double (pb->get_property ("next-padding"), 0);
    bottom_padding_ = 0;
    space_ = robust_scm2double (pb->get_property ("next-space"), 1.0);
    inverse_hooke_ = 1.0;
    break_permission_ = ly_symbol2scm ("allow");
    page_permission_ = pb->get_property ("page-break-permission");
    turn_permission_ = pb->get_property ("page-turn-permission");
    break_penalty_ = 0;
    page_penalty_ = robust_scm2double (pb->get_property ("page-break-penalty"), 0);
    turn_penalty_ = robust_scm2double (pb->get_property ("page-turn-penalty"), 0);
    title_ = to_boolean (pb->get_property ("is-title"));
    compressed_lines_count_ = 1;
    compressed_nontitle_lines_count_ = title_ ? 0 : 1;
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
class Constrained_breaking
{
public:
  vector<Column_x_positions> solve (vsize start, vsize end, vsize sys_count);
  vector<Column_x_positions> best_solution (vsize start, vsize end);
  vector<Line_details> line_details (vsize start, vsize end, vsize sys_count);

  Constrained_breaking (Paper_score *ps);
  Constrained_breaking (Paper_score *ps, vector<vsize> const &start_col_posns);

  int max_system_count (vsize start, vsize end);
  int min_system_count (vsize start, vsize end);

private:
  Paper_score *pscore_;
  vsize valid_systems_;
  vsize systems_;
  bool ragged_right_;
  bool ragged_last_;

  /* the (i,j)th entry is the configuration for breaking between
    columns i and j */
  Matrix<Line_details> lines_;

  /* the [i](j,k)th entry is the score for fitting the first k bars onto the
    first j systems, starting at the i'th allowed starting column */
  vector<Matrix<Constrained_break_node> > state_;

  vector<vsize> start_;         /* the columns at which we might be asked to start breaking */
  vector<vsize> starting_breakpoints_; /* the corresponding index in breaks_ */

  vector<Grob*> all_;
  vector<vsize> breaks_;

  void initialize ();
  void resize (vsize systems);

  Column_x_positions space_line (vsize start_col, vsize end_col);
  vsize prepare_solution (vsize start, vsize end, vsize sys_count);

  Real combine_demerits (Real force, Real prev_force);

  bool calc_subproblem(vsize start, vsize systems, vsize max_break_index);
};
#endif /* CONSTRAINED_BREAKING_HH */
