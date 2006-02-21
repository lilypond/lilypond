/*
  constrained-breaking.cc -- implement a line breaker that
  support limits on the number of systems

  source file of the GNU LilyPond music typesetter

  (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

/*
  TODO:

  * vsize vs. int: casts should not be necessary. Use VPOS iso -1 as
  magic signaling value?

  * The specification uses A j, k, n and m as variables.
  
  Functions use start,end,sys_count,calc_subproblem as variables. Use the same naming
  for the specification as for the code.
  

  FURTHER REMARKS:

  *
  
   int a;
   int b;

  iso.

   int a, b;


   * no spurious * in <slash><star> <star><slash> comments.


 */


#include "constrained-breaking.hh"

#include "international.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "simple-spacer.hh"
#include "system.hh"
#include "warn.hh"

void
print_constrained_break_nodes (vector<Constrained_break_node> const &arr)
{
  for (vsize i = 0; i < arr.size (); i++)
    {
      printf ("node %d: ", (int)i);
      arr[i].print ();
    }
}

/**
   We use the following optimal substructure. Let W(A) be our weight function.

   Let A_{k,n} = (a_{k,n,1}, ... a_{k,n,k}) be the optimal set of line breaks
   for k systems and n potential breakpoints. a_{k,n,k} = n (it is the end of
   the piece)

   Then A_{k+1, m} is contructed from
   min_ {k < j < m} ( W(A_{k,j} :: m) )
   where by A::m we denote appending m to the list A

*/

/* start and sys here are indexed from 0.

max_break is indexed from starting_breakpoints_[start] (for
 max_break, starting_breakpoints_[start] is the beginning of the
 piece; the smallest value we should ever see here is
 starting_breakpoints_[start] + 1) */
bool
Constrained_breaking::calc_subproblem (int start, int sys, int max_break)
{
  assert (sys < systems_);
  assert (start < (int)start_.size ());
  assert (max_break < (int)breaks_.size ());

  bool found_something = false;
  int start_col = starting_breakpoints_[start];
  vector<Constrained_break_node> &st = state_[start];
  int rank = breaks_.size () - start_col;
  int max_index = max_break - start_col;
  for (int j = sys; j < max_index; j++)
    {
      if (0 == sys && j > 0)
        break; /* the first line cannot have its first break after the beginning */

      Column_x_positions const &cur = cols_[(j + start_col)*cols_rank_ + max_break];
      Column_x_positions prev;
      Real prev_dem = 0;

      if (sys > 0)
        {
          prev = st[(sys-1) * rank + j].line_config_;
          prev_dem = st[(sys-1) * rank + j].demerits_;
        }
      if (isinf (prev_dem))
        break;

      Real dem, force, pen;
      combine_demerits(prev, cur, &force, &pen, &dem);
      dem += prev_dem;
      if (isinf (dem))
        continue;

      int k = sys*rank + max_index;
      if (isinf (st[k].demerits_)
          || dem < st[k].demerits_)
        {
          found_something = true;

	  /*
	    TODO:  maybe just copy a Constrained_break_node ? 
	   */
          st[k].demerits_ = dem;
          st[k].force_ = force;
          st[k].penalty_ = pen;
          st[k].prev_ = j;
          st[k].line_config_ = cur;
        }
    }
  return found_something;
}

vector<Column_x_positions>
Constrained_breaking::solve ()
{
  if (!systems_)
    {
      programming_error (_f ("no system number set in constrained-breaking"));
      systems_ = start_.size () / 2;
    }

  resize ();
  return get_solution (0, systems_, -1);
}

void
Constrained_breaking::resize ()
{
  if (!breaks_.size ())
    {
      bool ragged_right = to_boolean (pscore_->layout ()->c_variable ("ragged-right"));
      bool ragged_last = to_boolean (pscore_->layout ()->c_variable ("ragged-last"));

      /* do all the rod/spring problems */
      breaks_ = pscore_->find_break_indices ();
      cols_rank_ = breaks_.size ();
      all_ = pscore_->root_system ()->columns ();
      cols_.resize (breaks_.size () * breaks_.size ());
      for (vsize i = 0; i < breaks_.size () - 1; i++)
	for (vsize j = i + 1; j < breaks_.size (); j++)
	  {
	    vector<Grob*> line (all_.begin () + breaks_[i],
				all_.begin() + breaks_[j] + 1);

	    line[0] = dynamic_cast<Item *> (line[0])->find_prebroken_piece (RIGHT);
	    line.back () = dynamic_cast<Item *> (line.back ())->find_prebroken_piece (LEFT);

	    cols_[i*cols_rank_ + j].cols_ = line;

	    /* we have no idea what line this will be -- only whether it is the first */
	    Interval line_dims = line_dimensions_int (pscore_->layout (), i);
	    Simple_spacer_wrapper *sp = generate_spacing_problem (line, line_dims);

	    bool last = j == breaks_.size () - 1;
	    bool ragged = ragged_right || (last && ragged_last);
	    sp->solve (&cols_[i*cols_rank_ + j], ragged);

	    if (!cols_[i*cols_rank_ + j].satisfies_constraints_)
	      break;
	    delete sp;
	  }

      /* work out all the starting indices */
      for (vsize i = 0; i < start_.size (); i++)
        {
          vsize j;
          for (j = 0; j < breaks_.size () - 1 && breaks_[j] < start_[i]; j++)
            ;
          starting_breakpoints_.push_back (j);
          start_[i] = breaks_[j];
        }
      state_.resize (start_.size ());
    }

  for (vsize i = 0; i < state_.size (); i++)
    state_[i].resize((breaks_.size () - starting_breakpoints_[i]) * systems_);

  /* fill out the matrices */
  for (vsize i = 0; i < state_.size (); i++)
    for (int j = valid_systems_; j < systems_; j++)
      for (vsize k = starting_breakpoints_[i] + j + 1; k < breaks_.size (); k++)
        if (!calc_subproblem (i, j, k))
          break; /* if we couldn't break this, it is too cramped already */
  
  valid_systems_ = systems_;
}

vector<Column_x_positions>
Constrained_breaking::get_solution (int start, int end, int sys_count)
{
  int rank;
  int brk;
  prepare_solution (start, end, sys_count, &rank, &brk);

  vector<Constrained_break_node> const &st = state_[start];
  vector<Column_x_positions> ret;

  for (int sys = sys_count-1; sys >= 0; sys--)
    {
      assert (brk > 0);
      ret.push_back (st[sys*rank + brk].line_config_);
      brk = st[sys*rank + brk].prev_;
    }
  assert (brk == 0);

  reverse (ret);
  return ret;
}

Real
Constrained_breaking::get_demerits (int start, int end, int sys_count)
{
  int rank;
  int brk;
  prepare_solution (start, end, sys_count, &rank, &brk);

  return state_[start][(sys_count-1)*rank + brk].demerits_;
}

Real
Constrained_breaking::get_force (int start, int end, int sys_count)
{
  int rank;
  int brk;
  prepare_solution (start, end, sys_count, &rank, &brk);
  vector<Constrained_break_node> const &st = state_[start];
  Real f = 0;

  for (int sys = sys_count-1; sys >= 0 && brk >= 0; sys--)
    {
      f += fabs (st[sys*rank + brk].force_);
      brk = st[sys*rank + brk].prev_;
    }
  if (brk < 0)
    f = infinity_f;

  return f;
}

Real
Constrained_breaking::get_penalty (int start, int end, int sys_count)
{
  int rank;
  int brk;
  prepare_solution (start, end, sys_count, &rank, &brk);

  return state_[start][(sys_count-1)*rank + brk].penalty_;
}

Real
Constrained_breaking::get_page_penalty (int start, int end, int sys_count, int sys_num)
{
  int rank;
  int brk;
  prepare_solution (start, end, sys_count, &rank, &brk);

  int sys;
  for (sys = sys_count-1; sys > sys_num; sys--)
    brk = state_[start][sys*rank + brk].prev_;

  if (brk < 0) /* we didn't satisfy constraints */
    return 0;
  vector<Grob*> &cols = state_[start][sys*rank + brk].line_config_.cols_;
  if (cols.empty ())
    return 0;

  Grob *pc = cols.back ();
  if (pc->original ())
    {
      SCM pen = pc->get_property ("page-penalty");
      if (scm_is_number (pen) && fabs (scm_to_double (pen)) < 10000)
	return scm_to_double (pen);
    }
  return 0;
}

int
Constrained_breaking::get_min_systems (int start, int end)
{
  int rank;
  int brk;
  prepare_solution (start, end, 1, &rank, &brk);
  int sys_count;
  vector<Constrained_break_node> const &st = state_[start];

  /* sys_count < rank : rank is the # of breakpoints, we can't have more systems */
  for (sys_count = 0; sys_count < rank; sys_count++)
    {
      if (sys_count >= valid_systems_)
        {
          systems_ = sys_count + 3;
          resize ();
        }
      if (!isinf (st[sys_count*rank + brk].force_))
        return sys_count + 1;
    }
  /* no possible breaks satisfy constraints */
  return 0;
}

int
Constrained_breaking::get_max_systems (int start, int end)
{
  int brk = (end < 0 || end >= (int)start_.size ()) ? breaks_.size () - 1 : start_[end];
  return brk - starting_breakpoints_[start];
}

void
Constrained_breaking::prepare_solution (vsize start, int end, int sys_count, int *rank, int *brk)
{
  assert (start < start_.size () && end <= int (start_.size ()));
  assert (end < 0 || int (start) < end);
  assert (sys_count > 0);

  if (sys_count >= valid_systems_)
    {
      systems_ = sys_count;
      resize ();
    }
  if (end == (int)start_.size ())
    end = -1;

  *rank = breaks_.size () - starting_breakpoints_[start];
  *brk = end < 0 ? breaks_.size () - 1 : starting_breakpoints_[end];
  *brk -= starting_breakpoints_[start];
}

Constrained_breaking::Constrained_breaking ()
{
  valid_systems_ = systems_ = 0;
  start_.push_back (0);
}

Constrained_breaking::Constrained_breaking (vector<int> const &start)
  : start_ (start)
{
  valid_systems_ = systems_ = 0;
}

void
Constrained_breaking::combine_demerits (Column_x_positions const &prev,
                                        Column_x_positions const &col,
                                        Real *force,
                                        Real *penalty,
                                        Real *demerits) const
{
  *penalty = 0;
  if (col.cols_.empty () || !col.satisfies_constraints_)
    *force = infinity_f;
  else
    {
      *force = col.force_;

      Grob *pc = col.cols_.back ();
      if (pc->original ())
        {
          SCM pen = pc->get_property ("penalty");
          if (scm_is_number (pen) && fabs (scm_to_double (pen)) < 10000)
            *penalty += scm_to_double (pen);
        }
    }

  *demerits = (*force) * (*force) + abs (prev.force_ - *force) + *penalty;
}

