/*
  gourlay-breaking.cc -- implement Gourlay_breaking

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>		// rint
#include <stdio.h>

#include "gourlay-breaking.hh"
#include "column-x-positions.hh"
#include "warn.hh"
#include "main.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-def.hh"
#include "simple-spacer.hh"
#include "system.hh"

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
    demerits_f_ = 0;
  }
};

/**
  This algorithms is adapted from the OSU Tech report on breaking lines.

  this function is longish, but not very complicated.
  
 */
Array<Column_x_positions>
Gourlay_breaking::do_solve () const
{
  Array<Break_node> optimal_paths;
  Link_array<Grob> all =
    pscore_l_->line_l_->column_l_arr ();
  
  Array<int> breaks = find_break_indices ();
  
  Break_node first_node ;
  optimal_paths.push (first_node);

  Real worst_force = 0.0;
  
  for (int break_idx=1; break_idx< breaks.size (); break_idx++) 
    {
      /*
	start with a short line, add measures. At some point 
	the line becomes infeasible. Then we don't try to add more 
	*/
      int minimal_start_idx = -1;
      Column_x_positions minimal_sol;
      Column_x_positions backup_sol;
      
      Real minimal_demerits = infinity_f;

      bool ragged = to_boolean (pscore_l_->paper_l_->get_scmvar ("raggedright"));

      for (int start_idx = break_idx; start_idx--;)
	{
	  Link_array<Grob> line = all.slice (breaks[start_idx], breaks[break_idx]+1);
  
	  line[0]     = dynamic_cast<Item*> (line[0])    ->find_prebroken_piece (RIGHT);
	  line.top () = dynamic_cast<Item*> (line.top ())->find_prebroken_piece (LEFT);
	    
	  Column_x_positions cp;
	  cp.cols_ = line;

	  Interval line_dims
	    = pscore_l_->paper_l_->line_dimensions_int (optimal_paths[start_idx].line_i_);
	  Simple_spacer * sp = generate_spacing_problem (line, line_dims);
	  sp->solve (&cp, ragged);
	  delete sp;

	  if (fabs (cp.force_f_) > worst_force)
	    worst_force = fabs (cp.force_f_);

	  /*
	    We remember this solution as a "should always work
	    solution", in case everything fucks up.  */
	  if (start_idx == break_idx - 1)
	    backup_sol = cp;
	 	  
	  Real this_demerits;

	  if (optimal_paths[start_idx].demerits_f_ >= infinity_f)
	    this_demerits = infinity_f;
	  else
	    this_demerits = combine_demerits (optimal_paths[start_idx].line_config_, cp)
	      + optimal_paths[start_idx].demerits_f_;

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
	  if (!cp.satisfies_constraints_b_)
	    break ; 
	}


      Break_node bnod;
      if (minimal_start_idx < 0) 
	{
	  bnod.demerits_f_ = infinity_f;
	  bnod.line_config_ = backup_sol;
	  bnod.prev_break_i_ = break_idx - 1;	  
	}
      else 
	{
	  bnod.prev_break_i_ = minimal_start_idx;
	  bnod.demerits_f_ = minimal_demerits;
	  bnod.line_config_ = minimal_sol;
	}
      bnod.line_i_ = optimal_paths[bnod.prev_break_i_].line_i_ + 1;
      optimal_paths.push (bnod);
      
      if (! (break_idx % HAPPY_DOTS_I))
	progress_indication (String ("[") + to_str (break_idx) + "]");
    }

  /* do the last one */
  if (breaks.size () % HAPPY_DOTS_I)
    progress_indication (String ("[") + to_str (breaks.size()) + "]");    


  progress_indication ("\n");

  Array<int> final_breaks;
  Array<Column_x_positions> lines;

  /* skip 0-th element, since it is a "dummy" elt*/
  for (int i = optimal_paths.size ()-1; i> 0;) 
    {
      final_breaks.push (i);
      int prev = optimal_paths[i].prev_break_i_;
      assert (i > prev);
      i = prev;
    }

  if (verbose_global_b)
    printf("Optimal demerits: %f\n", optimal_paths.top().demerits_f_); 

  
  if (optimal_paths.top ().demerits_f_ >= infinity_f)
    warning (_ ("No feasible line breaking found"));
  
  for (int i= final_breaks.size (); i--;)
    {
      Column_x_positions cp (optimal_paths[final_breaks[i]].line_config_);
      
      lines.push (cp);
      if(!cp.satisfies_constraints_b_)
	warning ("Could not find line breaking that satisfies constraints.");
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
  Grob * pc = this_one.cols_.top ();
  if (pc->original_l_)
    {
      SCM pen = pc->get_grob_property ("penalty");
      if (gh_number_p (pen) && fabs (gh_scm2double (pen)) < 10000)
	{
	  break_penalties += gh_scm2double (pen);
	}
    }

#if 1
  /*
    Q: do want globally non-cramped lines, or locally equally cramped lines. 
   */
  Real demerit = abs (this_one.force_f_) + 0.1 *abs (prev.force_f_ - this_one.force_f_)
    + break_penalties;
#else
  Real demerit = abs (this_one.force_f_) + break_penalties;
#endif

   if (!this_one.satisfies_constraints_b_)
     {
       /*
	 If it doesn't satisfy constraints, we make this one
	 really unattractive.

	 add 20000 to the demerits, so that a break penalty
	 of -10000 won't change the result */
       demerit = (demerit + 20000) >? 2000;
       
       demerit *= 10;
     }

   return demerit;
}

