/*
  gourlay-breaking.cc -- implement Gourlay_breaking

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "gourlay-breaking.hh"

#include <cmath>		// rint
#include <cstdio>
using namespace std;

#include "international.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "simple-spacer.hh"
#include "system.hh"
#include "warn.hh"

/// How often to print operator pacification marks?
const int HAPPY_DOTS = 3;

/**
   Helper to trace back an optimal path
*/
struct Break_node
{
  /** this was the previous. If negative, this break should not be
      considered: this path has infinite energy

  */
  int prev_break_;
  /**
     Which system number so far?
  */
  int line_;

  Real demerits_;
  Column_x_positions line_config_;

  Break_node ()
  {
    prev_break_ = -1;
    line_ = 0;
    demerits_ = 0;
  }

  void print () const
  {
    printf ("prev break %d, line %d, demerits %f\n",
	    prev_break_, line_, demerits_);
  }
};

void
print_break_nodes (Array<Break_node> const &arr)
{
  for (int i = 0; i < arr.size (); i++)
    {
      printf ("node %d: ", i);
      arr[i].print ();
    }
}

/**
   This algorithms is adapted from the OSU Tech report on breaking lines.

   this function is longish, but not very complicated.

   TODO: should rewrite. See the function in scm/page-layout.scm for
   inspiration.
*/
Array<Column_x_positions>
Gourlay_breaking::do_solve () const
{
  Array<Break_node> optimal_paths;
  Link_array<Grob> all
    = pscore_->root_system ()->columns ();

  Array<int> breaks = find_break_indices ();

  Break_node first_node;
  optimal_paths.push (first_node);

  bool ragged_right = to_boolean (pscore_->layout ()->c_variable ("raggedright"));
  bool ragged_last = to_boolean (pscore_->layout ()->c_variable ("raggedlast"));

  Real worst_force = 0.0;
  for (int break_idx = 1; break_idx < breaks.size (); break_idx++)
    {
      /*
	start with a short line, add measures. At some point
	the line becomes infeasible. Then we don't try to add more
      */
      int minimal_start_idx = -1;
      Column_x_positions minimal_sol;
      Column_x_positions backup_sol;

      Real minimal_demerits = infinity_f;

      for (int start_idx = break_idx; start_idx--;)
	{
	  Link_array<Grob> line = all.slice (breaks[start_idx], breaks[break_idx] + 1);

	  line[0] = dynamic_cast<Item *> (line[0])->find_prebroken_piece (RIGHT);
	  line.top () = dynamic_cast<Item *> (line.top ())->find_prebroken_piece (LEFT);

	  Column_x_positions cp;
	  cp.cols_ = line;

	  Interval line_dims
	    = line_dimensions_int (pscore_->layout (), optimal_paths[start_idx].line_);
	  Simple_spacer_wrapper *sp = generate_spacing_problem (line, line_dims);
	  bool last_line = break_idx == breaks.size () - 1;
	  bool ragged = ragged_right
	    || (last_line && ragged_last);

	  sp->solve (&cp, ragged);

	  delete sp;

	  if (ragged && last_line)
	    cp.force_ = 0.0;

	  if (fabs (cp.force_) > worst_force)
	    worst_force = fabs (cp.force_);

	  /*
	    We remember this solution as a "should always work
	    solution", in case everything fucks up.  */
	  if (start_idx == break_idx - 1)
	    backup_sol = cp;

	  Real this_demerits;

	  if (optimal_paths[start_idx].demerits_ >= infinity_f)
	    this_demerits = infinity_f;
	  else
	    this_demerits = combine_demerits (optimal_paths[start_idx].line_config_, cp)
	      + optimal_paths[start_idx].demerits_;

	  if (this_demerits < minimal_demerits)
	    {
	      minimal_start_idx = start_idx;
	      minimal_sol = cp;
	      minimal_demerits = this_demerits;
	    }

	  /*
	    we couldn't satisfy the constraints, this won't get better
	    if we add more columns, so we get on with the next one
	  */
	  if (!cp.satisfies_constraints_)
	    break;
	}

      Break_node bnod;
      if (minimal_start_idx < 0)
	{
	  bnod.demerits_ = infinity_f;
	  bnod.line_config_ = backup_sol;
	  bnod.prev_break_ = break_idx - 1;
	}
      else
	{
	  bnod.prev_break_ = minimal_start_idx;
	  bnod.demerits_ = minimal_demerits;
	  bnod.line_config_ = minimal_sol;
	}
      bnod.line_ = optimal_paths[bnod.prev_break_].line_ + 1;
      optimal_paths.push (bnod);

      if (! (break_idx % HAPPY_DOTS))
	progress_indication (std::string ("[") + to_string (break_idx) + "]");
    }

  /* do the last one */
  if (breaks.size () % HAPPY_DOTS)
    progress_indication (std::string ("[") + to_string (breaks.size ()) + "]");

  progress_indication ("\n");

  Array<int> final_breaks;
  Array<Column_x_positions> lines;

  /* skip 0-th element, since it is a "dummy" elt*/
  for (int i = optimal_paths.size () - 1; i > 0;)
    {
      final_breaks.push (i);
      int prev = optimal_paths[i].prev_break_;
      assert (i > prev);
      i = prev;
    }

  if (be_verbose_global)
    {
      message (_f ("Optimal demerits: %f",
		   optimal_paths.top ().demerits_) + "\n");
    }

  if (optimal_paths.top ().demerits_ >= infinity_f)
    warning (_ ("no feasible line breaking found"));

  for (int i = final_breaks.size (); i--;)
    {
      Column_x_positions cp (optimal_paths[final_breaks[i]].line_config_);

      lines.push (cp);
      if (!cp.satisfies_constraints_)
	warning (_ ("can't find line breaking that satisfies constraints"));
    }
  return lines;
}

Gourlay_breaking::Gourlay_breaking ()
{
}

/*
  TODO: uniformity parameter to control rel. importance of spacing differences.

  TODO:

  mixing break penalties and constraint-failing solutions is confusing.
*/
Real
Gourlay_breaking::combine_demerits (Column_x_positions const &prev,
				    Column_x_positions const &this_one) const
{
  Real break_penalties = 0.0;
  Grob *pc = this_one.cols_.top ();
  if (pc->original ())
    {
      SCM pen = pc->get_property ("penalty");
      if (scm_is_number (pen) && fabs (scm_to_double (pen)) < 10000)
	break_penalties += scm_to_double (pen);
    }

  /*
    Q: do we want globally non-cramped lines, or locally equally
    cramped lines?

    There used to be an example file input/test/uniform-breaking to
    demonstrate problems with this approach. When music is gradually
    becoming denser, the uniformity requirement makes lines go from
    cramped to even more cramped (because going from cramped
    3meas/line to relatively loose 2meas/line is such a big step.

  */

  Real demerit = abs (this_one.force_) + abs (prev.force_ - this_one.force_)
    + break_penalties;

  if (!this_one.satisfies_constraints_)
    {
      /*
	If it doesn't satisfy constraints, we make this one
	really unattractive.

	add 20000 to the demerits, so that a break penalty
	of -10000 won't change the result */
      demerit = max ((demerit + 20000), 2000.0);

      demerit *= 10;
    }

  return demerit;
}

