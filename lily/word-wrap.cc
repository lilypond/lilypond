/*
  word-wrap.cc -- implement Word_wrap

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "word-wrap.hh"
#include "paper-def.hh"
#include "p-score.hh"
#include "debug.hh"
#include "p-col.hh"
#include "spring-spacer.hh"


/** el stupido.


   A Dynamic Programming type of algorithm
   similar to TeX's is in Gourlay_breaking

   */
Array<Col_hpositions>
Word_wrap::do_solve() const
{
  problem_OK();

  PCursor<Paper_column*> curcol (pscore_l_->col_p_list_.top());
  Array<Col_hpositions> breaking;
  Line_of_cols breakpoints (find_breaks());
  assert (breakpoints.size()>=2);

  int break_idx_i=0;
  int line_no_i = 0;
  while (break_idx_i < breakpoints.size() -1)
    {
      Col_hpositions minimum;
      Col_hpositions current;

      // do  another line
      line_no_i ++;
      Paper_column *post = breakpoints[break_idx_i]->postbreak_l();
      int start_break_idx = break_idx_i;
      current.add (post);
      curcol++;		// skip the breakable.
      break_idx_i++;

      while (break_idx_i < breakpoints.size())
	{

	  // add another measure.
	  while (breakpoints[break_idx_i] != curcol.ptr())
	    {
	      current.add (curcol);
	      curcol++;
	    }
	  current.add (breakpoints[break_idx_i]->prebreak_l());

	  current.spacer_l_ = generate_spacing_problem (current.cols, 
	    pscore_l_->paper_l_->line_dimensions_int (line_no_i));

	  // try to solve
	  if (!feasible (current.cols))
	    {
	      if (!minimum.cols.size())
		{
		  warning (_("Ugh, this measure is too long, breakpoint: ")
			   + String (break_idx_i) +
			   _(" (generating stupido solution)"));
		  current.stupid_solution();
		  current.energy_f_ = - 1; // make sure we break out.
		}
	      else
		current.energy_f_ = infinity_f;	// make sure we go back
	    }
	  else
	    {
	      current.solve_line();
	      current.print();
	    }

	  delete current.spacer_l_;
	  current.spacer_l_ =0;

	  if (!current.satisfies_constraints_b_ && start_break_idx == break_idx_i - 1)
	    {
	      warning ( _ ("I don't fit.  Put me on Montignac"));
	      minimum = current;
	      break;
	    }

	  if (current.energy_f_ < minimum.energy_f_ || current.energy_f_ < 0)
	    {
	      minimum = current;
	    }
	  else {		// we're one col too far.
	    break_idx_i--;
	    while (curcol.ptr() != breakpoints[break_idx_i])
	      curcol --;
	    break;		// do the next line.
	  }


	  // add nobreak version of breakable column
	  current.cols.top()=breakpoints[break_idx_i];
	  curcol ++;
	  break_idx_i++;
	}

      *mlog << "[" <<break_idx_i<<"]"<<flush;
      breaking.push (minimum);
    }
  return breaking;
}

Word_wrap::Word_wrap()
{
  get_line_spacer = Spring_spacer::constructor;
}
