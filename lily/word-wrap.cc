/*
  word-wrap.cc -- implement Word_wrap

  source file of the LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "word-wrap.hh"
#include "paper-def.hh"
#include "paper-score.hh"
#include "debug.hh"
#include "paper-column.hh"
#include "spring-spacer.hh"


/** El stupido.  Add a measure until we're past the optimum.


   A Dynamic Programming type of algorithm
   similar to TeX's is in Gourlay_breaking


   UGH.  Should think about pre/post break columns.
   */
Array<Column_x_positions>
Word_wrap::do_solve () const
{
  problem_OK ();

  Line_of_cols &allcols (pscore_l_->col_l_arr_);
  int curcol_idx = 0;
  
  Array<Column_x_positions> breaking;
  Line_of_cols breakpoints (find_breaks ());
  assert (breakpoints.size ()>=2);

  int break_idx=0;
  int line_no = 0;
  while (break_idx < breakpoints.size () -1)
    {
      Column_x_positions minimum;
      Column_x_positions current;


      // do  another line
      line_no ++;
      Item *post = breakpoints[break_idx]->find_prebroken_piece (RIGHT);
      Paper_column *postcol =dynamic_cast<Paper_column*>(post);
      
      int start_break_idx = break_idx;
      current.add_paper_column (postcol);
      curcol_idx++;		// skip the breakable.
      break_idx++;

      while (break_idx < breakpoints.size ())
	{
	  // add another measure.
	  while (breakpoints[break_idx] != allcols[curcol_idx])
	    {
	      current.add_paper_column (allcols[curcol_idx]);
	      curcol_idx++;
	    }

	  Item * pre = breakpoints[break_idx]->find_prebroken_piece (LEFT);
	  Paper_column* precol = dynamic_cast<Paper_column*>(pre);
	  current.add_paper_column (precol);

	  current.spacer_l_ = generate_spacing_problem (current.cols_, 
	    pscore_l_->paper_l_->line_dimensions_int (line_no));

	  // try to solve
	  if (!feasible (current.cols_))
	    {
	      if (!minimum.cols_.size ())
		{
		  warning (_ ("ugh, this measure is too long") 
		    + ", " + _f ("breakpoint: %d", break_idx) 
		    + "(" + _ ("generating stupido solution") + ")");
		  current.stupid_solution ();
		  current.energy_f_ = - 1; // make sure we break out.
		}
	      else
		current.energy_f_ = infinity_f;	// make sure we go back
	    }
	  else
	    {
	      current.solve_line ();
	      current.print ();
	    }

	  delete current.spacer_l_;
	  current.spacer_l_ =0;

	  if (!current.satisfies_constraints_b_ && start_break_idx == break_idx - 1)
	    {
	      warning ( _ ("I don't fit; put me on Montignac"));
	      minimum = current;
	      break;
	    }

	  /*
	    UGR! bug! 
	   */
	  if (current.energy_f_ < minimum.energy_f_ || current.energy_f_ < 0)
	    {
	      minimum = current;
	    }
	  else
	    {		// we're one col too far.
	      break_idx--;
	      while (allcols[curcol_idx] != breakpoints[break_idx])
		curcol_idx --;
	      break;		// do the next line.
	    }


	  // add nobreak version of breakable column
	  current.cols_.top ()=breakpoints[break_idx];
	  curcol_idx ++;
	  break_idx++;
	}

      *mlog << "[" << break_idx << "]" << flush;
      breaking.push (minimum);
    }
  return breaking;
}

Word_wrap::Word_wrap ()
{
  get_line_spacer = Spring_spacer::constructor;
}
