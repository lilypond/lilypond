/*
  gourlay-breaking.cc -- implement Gourlay_breaking

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "gourlay-breaking.hh"
#include "column-x-positions.hh"
#include "debug.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-def.hh"
#include "simple-spacer.hh"

#include "killing-cons.tcc"

/// How often to print operator pacification marks?
const int HAPPY_DOTS_I = 3;

/**
  Helper to trace back an optimal path 
 */
struct Break_node {
  /** this was the previous. If negative, this break should not be
    considered: this path has infinite energy
    
    */
  int prev_break_i_;
  /**
     Which system number so far?
   */
  int line_i_;

  Real demerits_f_;
  Column_x_positions line_config_;
  
  Break_node () 
  {
    prev_break_i_ = -1;
    line_i_ = 0;
  }
};

/**
  This algorithms is adapted from the OSU Tech report on breaking lines.
 */
Array<Column_x_positions>
Gourlay_breaking::do_solve () const
{
  Array<Break_node> optimal_paths;
  Line_of_cols all = pscore_l_->col_l_arr_ ;
  Array<int> breaks = find_break_indices ();
  
  optimal_paths.set_size (breaks.size ());

  Break_node first_node ;
  first_node.prev_break_i_ = -1;
  first_node.line_config_.force_f_ = 0;
  first_node.line_config_.energy_f_ = 0;  
  first_node.line_i_ = 0;
  
  optimal_paths[0] = first_node; 
  int break_idx=1;

  for (; break_idx< breaks.size (); break_idx++) 
    {
      /*
	start with a short line, add measures. At some point 
	the line becomes infeasible. Then we don't try to add more 
	*/
      int minimal_start_idx = -1;
      Column_x_positions minimal_sol;
      Real minimal_demerits = infinity_f;

      for (int start_idx = break_idx; start_idx--;)
	{
#if 0
	  if  (break_idx - start_idx > max_measures_i_) 
	    break;
#endif
	  
	  if (optimal_paths[start_idx].prev_break_i_ < 0
	      && optimal_paths[start_idx].line_config_.energy_f_)
	    continue;


	  
	  Line_of_cols line = all.slice (breaks[start_idx], breaks[break_idx]+1);
  
	  line[0] = dynamic_cast<Paper_column*>(line[0]->find_broken_piece (RIGHT));
	  line.top () =  dynamic_cast<Paper_column*>(line.top ()->find_broken_piece (LEFT));
	    
	  if (!feasible (line))
	    break;
	    
	  Column_x_positions cp;
	  cp.cols_ = line;

	  Interval line_dims
	    = pscore_l_->paper_l_->line_dimensions_int (optimal_paths[start_idx].line_i_);
	  Simple_spacer * sp = generate_spacing_problem (line, line_dims);
	  sp->solve (&cp);
	  delete sp;
	  
	  if (!cp.satisfies_constraints_b_)
	    break;

	  Real this_demerits 
	    = combine_demerits (optimal_paths[start_idx].line_config_, cp)
	    + optimal_paths[start_idx].demerits_f_;

	  if (this_demerits < minimal_demerits) 
	    {
	      minimal_start_idx = start_idx;
	      minimal_sol = cp;
	      minimal_demerits = this_demerits;
	    }
	}

      if (minimal_start_idx < 0) 
	{
	  optimal_paths[break_idx].prev_break_i_ = -1;
	  optimal_paths[break_idx].line_config_.energy_f_ = infinity_f;
	}
      else 
	{
	  optimal_paths[break_idx].prev_break_i_ = minimal_start_idx;
	  optimal_paths[break_idx].line_config_ = minimal_sol;
	  optimal_paths[break_idx].demerits_f_ = minimal_demerits;
	  optimal_paths[break_idx].line_i_ =
	    optimal_paths[minimal_start_idx].line_i_ + 1;
	}

      if (! (break_idx % HAPPY_DOTS_I))
	*mlog << "[" << break_idx << "]" << flush;
    }

  /* do the last one */
  if  (break_idx % HAPPY_DOTS_I) 
    *mlog << "[" << break_idx << "]";

  *mlog << endl;

  Array<int> final_breaks;
  Array<Column_x_positions> lines;

  /* skip 0-th element, since it is a "dummy" elt*/
  for (int i = optimal_paths.size ()-1; i> 0;) 
    {
      final_breaks.push (i);
      assert (i > optimal_paths[i].prev_break_i_);

      // there was no "feasible path"
      if (!optimal_paths[i].line_config_.config_.size ()) {
	final_breaks.set_size (0);
	break;
      }
      i = optimal_paths[i].prev_break_i_;
    }


  for (int i= final_breaks.size (); i--;) 
    lines.push (optimal_paths[final_breaks[i]].line_config_);
  
  return lines;
}


Gourlay_breaking::Gourlay_breaking ()
{
  energy_bound_f_ = infinity_f;
  max_measures_i_ = INT_MAX;
}

void
Gourlay_breaking::do_set_pscore ()
{
  max_measures_i_ =int (rint (pscore_l_->paper_l_->get_var ("gourlay_maxmeasures")));
}


/*
  TODO: uniformity parameter to control rel. importance of spacing differences.
 */
Real
Gourlay_breaking::combine_demerits (Column_x_positions const &prev,
				    Column_x_positions const &this_one) const
{
  return abs (this_one.force_f_) + abs (prev.force_f_ - this_one.force_f_);
}
