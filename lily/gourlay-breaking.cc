/*
  gourlay-breaking.cc -- implement Gourlay_breaking

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "gourlay-breaking.hh"
#include "colhpos.hh"
#include "spring-spacer.hh"
#include "debug.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "paper-def.hh"

const int HAPPY_DOTS_I = 3;

/**
  Helper to trace back an optimal path 
 */
struct Break_node {
  /** this was the previous. If negative, this break should not be
    considered: this path has infinite energy
    
    */
  int prev_break_i_;
  int line_i_;
  Real energy_f_;
  Column_x_positions line_config_;
  Break_node () 
  {
    prev_break_i_ = -1;
    line_i_ = 0;
  }
};

/**
  This algorithms is adapted from 
 */

Array<Column_x_positions>
Gourlay_breaking::do_solve () const
{
  Array<Break_node> optimal_paths;
  Line_of_cols all = all_cols ();
  Array<int> breaks = find_break_indices ();
  
  optimal_paths.set_size (breaks.size ());

  Break_node first_node ;
  first_node.prev_break_i_ = -1;
  first_node.line_config_.energy_f_ = 0;
  first_node.line_i_ = 0;
  
  optimal_paths[0] = first_node; 
  int break_idx=1;

  
  for (; break_idx< breaks.size (); break_idx++) 
    {
      Array<int> candidates;
      Array<Column_x_positions> candidate_lines;
      Pointer_list<Line_spacer*> spacer_p_list;
	
      /*
	start with a short line, add measures. At some point 
	the line becomes infeasible. Then we don't try to add more 
	*/
      for (int start_idx = break_idx; start_idx--;)
	{
	  if  (break_idx - start_idx > max_measures_i_) 
	    break;

	  if (optimal_paths[start_idx].prev_break_i_ < 0
	      && optimal_paths[start_idx].line_config_.energy_f_)
		
	    continue;
	    
	  Line_of_cols line = all.slice (breaks[start_idx], breaks[break_idx]+1);

	  line[0] = line[0]->postbreak_l ();
	  line.top () = line.top ()->prebreak_l ();
	    
	  if (!feasible (line))
	    break;
	    
	  Column_x_positions approx;
	  approx.cols = line;
	    
	  approx.spacer_l_ = generate_spacing_problem (line, 
	    pscore_l_->paper_l_->line_dimensions_int (optimal_paths[start_idx].line_i_));
	  spacer_p_list.bottom ().add (approx.spacer_l_);

	  ( (Break_algorithm*)this)->approx_stats_.add (approx.cols);
	  approx.approximate_solve_line ();
	    
	  if  (approx.energy_f_  > energy_bound_f_)
	    {
	      continue;
	    }

	    
	  // this is a likely candidate. Store it.
	  candidate_lines.push (approx);
	  candidates.push (start_idx);
	}

	    
      int minimal_j = -1;
      Real minimal_energy = infinity_f;
      for (int j=0; j < candidates.size (); j++) 
	{
	  int start = candidates[j];
	  if (optimal_paths[start].line_config_.energy_f_
	       + candidate_lines[j].energy_f_  > minimal_energy)
		
	    continue;

	  if (!candidate_lines[j].satisfies_constraints_b_) 
	    {
	      candidate_lines[j].solve_line ();
	      ( (Break_algorithm*)this)->exact_stats_.add (candidate_lines[j].cols);
	    }
	    
	  Real this_energy 
	    = optimal_paths[start].line_config_.energy_f_ 
	    + candidate_lines[j].energy_f_ ;
	    
	  if (this_energy < minimal_energy) 
	    {
	      minimal_j = j;
	      minimal_energy = this_energy;
	    }
	}

      if (minimal_j < 0) 
	{
	  optimal_paths[break_idx].prev_break_i_ = -1;
	  optimal_paths[break_idx].line_config_.energy_f_ = infinity_f;
	}
      else 
	{
	  optimal_paths[break_idx].prev_break_i_ = candidates[minimal_j];
	  optimal_paths[break_idx].line_config_ = candidate_lines[minimal_j];
	  optimal_paths[break_idx].line_i_ = 
	    optimal_paths[optimal_paths[break_idx].prev_break_i_].line_i_ + 1;
	}

      if (! (break_idx % HAPPY_DOTS_I))
	*mlog << "[" << break_idx << "]" << flush;
    }

  if  (break_idx % HAPPY_DOTS_I) 
    *mlog << "[" << break_idx << "]" << flush;

  Array<int> final_breaks;

  Array<Column_x_positions> lines;

  /* skip 0-th element, since it is a "dummy" elt*/
  for (int i = optimal_paths.size ()-1; i> 0;) 
    {
      final_breaks.push (i);
      assert (i > optimal_paths[i].prev_break_i_);

      // there was no "feasible path"
      if (!optimal_paths[i].line_config_.config.size ()) {
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
  get_line_spacer = Spring_spacer::constructor;
  energy_bound_f_ = infinity_f;
  max_measures_i_ = INT_MAX;
}

void
Gourlay_breaking::do_set_pscore ()
{
  energy_bound_f_ = pscore_l_->paper_l_->get_var ("gourlay_energybound");
  max_measures_i_ =int (rint (pscore_l_->paper_l_->get_var ("gourlay_maxmeasures")));
}

