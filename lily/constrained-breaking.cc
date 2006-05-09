/*
  constrained-breaking.cc -- implement a line breaker that
  support limits on the number of systems

  source file of the GNU LilyPond music typesetter

  (c) 2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

/*
  We use the following optimal substructure. Let W(A) be our weight function.

  Let A_{k,n} = (a_{k,n,1}, ... a_{k,n,k}) be the optimal set of line breaks
  for k systems and n potential breakpoints. a_{k,n,k} = n (it is the end of
  the piece)

  Then A_{k+1, m} is contructed from
  min_ {k < j < m} ( W(A_{k,j} :: m) )
  where by A::m we denote appending m to the list A

  Indices in the code:

  The above algorithm makes it easy to end at a point before the end of the
  score (just find A_{k,m} for some m < breaks_.size () - 1). However, we must
  add information for starting at a point after the beginning. One constructor
  allows the specification of a list of starting columns, start_. We then have
  start_.size () different solution arrays. state_[i] is the array for the
  solution starting at column number start_[i].

  The indicies "start" and "end" refer to the index in the start_ array of the
  desired starting and ending columns.

  each solution array looks like
   a_{1,1,1} a_{2,1,2} a_{3,1,3} . . .
       X     a_{2,2,2} a_{3,2,3} . . .
       X         X     a_{3,3,3} . . .
       .         .         .     .
       .         .         .       .
  where the X's mark invalid solutions (can't have more systems than
  breakpoints). Note that each value is of the form a_{x,n,x}. This is because
  a breakpoint of the form a_{x,n,x-1} will also be called a_{x-1,m,x-1} for
  some m < n. Each cell in the array stores the value of its m (ie. the
  ending breakpoint of the previous line) as "prev_".

  For finding A_{sys, brk}, let "me" be the (sys_count,brk) cell in our
  solution array (state_[start][sys * rank + brk]).

  Then A_{sys, brk} = A_{sys - 1, me.prev_} :: me
*/

/*
  start and sys here are indexed from 0.
  brk is indexed from starting_breakpoints_[start]
  (for brk, starting_breakpoints_[start] is the beginning
  of the piece; the smallest value we should ever see here is
  starting_breakpoints_[start] + 1) */
bool
Constrained_breaking::calc_subproblem (vsize start, vsize sys, vsize brk)
{
  assert (sys < systems_);
  assert (start < start_.size ());
  assert (brk < breaks_.size ());

  bool found_something = false;
  vsize start_col = starting_breakpoints_[start];
  vector<Constrained_break_node> &st = state_[start];
  vsize rank = breaks_.size () - start_col;
  vsize max_index = brk - start_col;
  for (vsize j=sys; j < max_index; j++)
    {
      if (0 == sys && j > 0)
        break; /* the first line cannot have its first break after the beginning */

      Line_details const &cur = lines_[(j + start_col)*lines_rank_ + brk];
      Real prev_f = 0;
      Real prev_dem = 0;

      if (sys > 0)
        {
          prev_f = st[(sys-1) * rank + j].details_.force_;
          prev_dem = st[(sys-1) * rank + j].demerits_;
        }
      if (isinf (prev_dem))
        break;

      Real dem = combine_demerits (cur.force_, prev_f) + prev_dem + cur.break_penalty_;
      if (isinf (dem))
        continue;

      int k = sys*rank + max_index;
      if (isinf (st[k].demerits_) || dem < st[k].demerits_)
        {
          found_something = true;
          st[k].demerits_ = dem;
          st[k].details_ = cur;
          st[k].prev_ = j;
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
      systems_ = breaks_.size () / 4;
    }

  resize (systems_);
  return get_solution(0, VPOS, systems_);
}

Column_x_positions
Constrained_breaking::space_line (vsize i, vsize j)
{
  bool ragged_right = to_boolean (pscore_->layout ()->c_variable ("ragged-right"));
  bool ragged_last = to_boolean (pscore_->layout ()->c_variable ("ragged-last"));
  Column_x_positions col;

  vector<Grob*> line (all_.begin () + breaks_[i],
		      all_.begin() + breaks_[j] + 1);
  Interval line_dims = line_dimensions_int (pscore_->layout (), i);
  bool last = j == breaks_.size () - 1;
  bool ragged = ragged_right || (last && ragged_last);

  return get_line_configuration (line, line_dims[RIGHT] - line_dims[LEFT], line_dims[LEFT], ragged);
}

void
Constrained_breaking::resize (vsize systems)
{
  systems_ = systems;

  if (!breaks_.size () && pscore_)
    {
      Output_def *l = pscore_->layout ();
      Real extent = scm_to_double (l->c_variable ("system-height"));
      Real padding = scm_to_double (l->c_variable ("between-system-padding"));
      Real space = scm_to_double (l->c_variable ("between-system-space"));
      bool ragged_right = to_boolean (pscore_->layout ()->c_variable ("ragged-right"));
      bool ragged_last = to_boolean (pscore_->layout ()->c_variable ("ragged-last"));

      Interval first_line = line_dimensions_int (pscore_->layout (), 0);
      Interval other_lines = line_dimensions_int (pscore_->layout (), 1);
      /* do all the rod/spring problems */
      breaks_ = pscore_->find_break_indices ();
      lines_rank_ = breaks_.size ();
      all_ = pscore_->root_system ()->columns ();
      lines_.resize (breaks_.size () * breaks_.size ());
      vector<Real> forces = get_line_forces (all_,
					     breaks_,
					     other_lines.length (),
					     other_lines.length () - first_line.length (),
					     ragged_right);
      for (vsize i = 0; i < breaks_.size () - 1; i++)
	{
          for (vsize j = i + 1; j < breaks_.size (); j++)
            {
	      bool last = j == breaks_.size () - 1;
	      bool ragged = ragged_right || (last && ragged_last);
              int k = i*lines_rank_ + j;
	      SCM pen = all_[breaks_[j]]->get_property ("line-break-penalty");
	      if (scm_is_number (pen))
		lines_[k].break_penalty_ = scm_to_double (pen);

              lines_[k].force_ = forces[k];
              lines_[k].extent_ = extent;
              lines_[k].padding_ = padding;
              lines_[k].space_ = space;
              lines_[k].inverse_hooke_ = 3; // FIXME: somewhat arbitrary
	      if (ragged && lines_[k].force_ < 0)
		lines_[k].force_ = infinity_f;
              if (isinf (lines_[k].force_))
                break;
            }
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

  if (pscore_ && systems_ > valid_systems_)
    {
      for (vsize i = 0; i < state_.size (); i++)
        state_[i].resize((breaks_.size () - starting_breakpoints_[i]) * systems_);

      /* fill out the matrices */
      for (vsize i = 0; i < state_.size (); i++)
        for (vsize j = valid_systems_; j < systems_; j++)
          for (vsize k = starting_breakpoints_[i] + j + 1; k < breaks_.size (); k++)
            if (!calc_subproblem (i, j, k))
              break; /* if we couldn't break this, it is too cramped already */
      valid_systems_ = systems_;
    }
}

vector<Column_x_positions>
Constrained_breaking::get_solution (vsize start, vsize end, vsize sys_count)
{
  vsize rank;
  vsize end_brk;
  vsize start_brk = starting_breakpoints_[start];
  prepare_solution (start, end, sys_count, &rank, &end_brk);

  vector<Constrained_break_node> const &st = state_[start];
  vector<Column_x_positions> ret;

  /* find the first solution that satisfies constraints */
  for (vsize sys = sys_count-1; sys != VPOS; sys--)
    {
      for (vsize brk = end_brk; brk != VPOS; brk--)
        {
          if (!isinf (st[sys*rank + brk].details_.force_))
            {
              if (brk != end_brk)
                {
                  warning ( _("couldn't find line breaking that satisfies constraints" ));
                  ret.push_back (space_line (brk, end_brk));
                }
              /* build up the good solution */
              for (vsize cur_sys = sys; cur_sys != VPOS; cur_sys--)
                {
		  vsize prev_brk = st[cur_sys*rank + brk].prev_;
                  assert (brk != VPOS);
                  ret.push_back (space_line (prev_brk + start_brk, brk + start_brk));
                  brk = prev_brk;
                }
              reverse (ret);
              return ret;
            }
        }
    }
  /* if we get to here, just put everything on one line */
  warning ( _("couldn't find line breaking that satisfies constraints" ));
  ret.push_back (space_line (0, end_brk));
  return ret;
}

std::vector<Line_details>
Constrained_breaking::get_details (vsize start, vsize end, vsize sys_count)
{
  vsize rank;
  vsize brk;
  prepare_solution (start, end, sys_count, &rank, &brk);
  vector<Constrained_break_node> const &st = state_[start];
  vector<Line_details> ret;

  for (int sys = sys_count-1; sys >= 0 && brk != VPOS; sys--)
    {
      ret.push_back (st[sys*rank + brk].details_);
      brk = st[sys*rank + brk].prev_;
    }
  return ret;
}

int
Constrained_breaking::get_min_systems (vsize start, vsize end)
{
  vsize rank;
  vsize brk;
  vsize sys_count;

  prepare_solution (start, end, 1, &rank, &brk);
  vector<Constrained_break_node> const &st = state_[start];

  /* sys_count < rank : rank is the # of breakpoints, we can't have more systems */
  for (sys_count = 0; sys_count < rank; sys_count++)
    {
      if (sys_count >= valid_systems_)
        {
          resize (sys_count + 3);
        }
      if (!isinf (st[sys_count*rank + brk].details_.force_))
        return sys_count + 1;
    }
  /* no possible breaks satisfy constraints */
  return 1;
}

int
Constrained_breaking::get_max_systems (vsize start, vsize end)
{
  vsize brk = (end >= start_.size ()) ? breaks_.size () - 1 : starting_breakpoints_[end];
  return brk - starting_breakpoints_[start];
}

void
Constrained_breaking::prepare_solution (vsize start, vsize end, vsize sys_count, vsize *rank, vsize *brk)
{
  assert (start < start_.size () && (end == VPOS || end <= start_.size ()));
  assert (start < end);

  resize (sys_count);
  if (end == start_.size ())
    end = VPOS;

  *rank = breaks_.size () - starting_breakpoints_[start];
  *brk = end == VPOS ? breaks_.size () - 1 : starting_breakpoints_[end];
  *brk -= starting_breakpoints_[start];
}

Constrained_breaking::Constrained_breaking ()
{
  valid_systems_ = systems_ = 0;
  start_.push_back (0);
}

Constrained_breaking::Constrained_breaking (vector<vsize> const &start)
  : start_ (start)
{
  valid_systems_ = systems_ = 0;
}

Real
Constrained_breaking::combine_demerits (Real force, Real prev_force)
{
  return force * force + fabs (prev_force - force);
}

