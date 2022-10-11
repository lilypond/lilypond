/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Joe Neeman <joeneeman@gmail.com>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "constrained-breaking.hh"

#include "international.hh"
#include "output-def.hh"
#include "page-layout-problem.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "simple-spacer.hh"
#include "system.hh"
#include "warn.hh"

#include <algorithm>
#include <vector>

using std::vector;

/*
  We use the following optimal substructure. Let W (A) be our weight function.

  Let A_{k, n} = (a_{k, n, 1}, ... a_{k, n, k}) be the optimal set of line breaks
  for k systems and n potential breakpoints. a_{k, n, k} = n (it is the end of
  the piece)

  Then A_{k+1, m} is contructed from
  min_ {k < j < m} ( W (A_{k, j} :: m) )
  where by A::m we denote appending m to the list A

  Indices in the code:

  The above algorithm makes it easy to end at a point before the end of the
  score (just find A_{k, m} for some m < breaks_.size () - 1). However, we must
  add information for starting at a point after the beginning. One constructor
  allows the specification of a list of starting columns, start_. We then have
  start_.size () different solution arrays. state_[i] is the array for the
  solution starting at column number start_[i].

  The indices "start" and "end" refer to the index in the start_ array of the
  desired starting and ending columns.

  each solution array looks like
   a_{1,1,1} a_{2,1,2} a_{3,1,3} . . .
       X     a_{2,2,2} a_{3,2,3} . . .
       X         X     a_{3,3,3} . . .
       .         .         .     .
       .         .         .       .
  where the X's mark invalid solutions (can't have more systems than
  breakpoints). Note that each value is of the form a_{x, n, x}. This is because
  a breakpoint of the form a_{x, n, x-1} will also be called a_{x-1, m, x-1} for
  some m < n. Each cell in the array stores the value of its m (ie. the
  ending breakpoint of the previous line) as "prev_".

  For finding A_{sys, brk}, let "me" be the (sys_count, brk) cell in our
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
  Matrix<Constrained_break_node> &st = state_[start];
  vsize max_index = brk - start_col;
  for (vsize j = max_index; j-- > sys;)
    {
      if (0 == sys && j > 0)
        continue; /* the first line cannot have its first break after the beginning */

      Line_details const &cur = lines_.at (brk, j + start_col);
      if (std::isinf (cur.force_))
        break;

      Real prev_f = 0;
      Real prev_dem = 0;

      if (sys > 0)
        {
          prev_f = st.at (j, sys - 1).details_.force_;
          prev_dem = st.at (j, sys - 1).demerits_;
        }
      if (std::isinf (prev_dem))
        continue;

      Real dem
        = combine_demerits (cur.force_, prev_f) + prev_dem + cur.break_penalty_;
      Constrained_break_node &n = st.at (max_index, sys);
      if (dem < n.demerits_)
        {
          found_something = true;
          n.demerits_ = dem;
          n.details_ = cur;
          n.prev_ = j;
        }
    }
  return found_something;
}

Column_x_positions
Constrained_breaking::space_line (vsize i, vsize j)
{
  bool ragged_right
    = from_scm<bool> (pscore_->layout ()->c_variable ("ragged-right"));
  bool ragged_last
    = from_scm<bool> (pscore_->layout ()->c_variable ("ragged-last"));

  // TODO: Unnecessary copy.  Could pass iterators/indices to
  // get_line_configuration().  What is the real cost?
  vector<Paper_column *> const line (all_.begin () + breaks_[i],
                                     all_.begin () + breaks_[j] + 1);
  Interval line_dims = line_dimension_interval (pscore_->layout (), i);
  bool last = j == breaks_.size () - 1;
  bool ragged = ragged_right || (last && ragged_last);

  /* As a special case, if there is only one line in the score and ragged-right
     hasn't been specifically forbidden and the line is stretched, use
     ragged spacing. */
  if (last && i == 0 && lines_.at (i, j).force_ >= 0
      && !scm_is_bool (pscore_->layout ()->c_variable ("ragged-right"))
      && !scm_is_bool (pscore_->layout ()->c_variable ("ragged-last")))
    ragged = true;

  return get_line_configuration (line, line_dims[RIGHT] - line_dims[LEFT],
                                 line_dims[LEFT], ragged);
}

void
Constrained_breaking::resize (vsize systems)
{
  systems_ = systems;

  if (pscore_ && systems_ > valid_systems_)
    {
      for (vsize i = 0; i < state_.size (); i++)
        state_[i].resize (breaks_.size () - starting_breakpoints_[i], systems_,
                          Constrained_break_node ());

      /* fill out the matrices */
      for (vsize i = 0; i < state_.size (); i++)
        for (vsize j = valid_systems_; j < systems_; j++)
          for (vsize k = starting_breakpoints_[i] + j + 1; k < breaks_.size ();
               k++)
            if (!calc_subproblem (i, j, k))
              break; /* if we couldn't break this, it is too cramped already */
      valid_systems_ = systems_;
    }
}

vector<Column_x_positions>
Constrained_breaking::solve (vsize start, vsize end, vsize sys_count)
{
  vsize start_brk = starting_breakpoints_[start];
  vsize end_brk = prepare_solution (start, end, sys_count);

  Matrix<Constrained_break_node> const &st = state_[start];
  vector<Column_x_positions> ret;

  /* find the first solution that satisfies constraints */
  for (vsize sys = sys_count - 1; sys != VPOS; sys--)
    {
      for (vsize brk = end_brk; brk != VPOS; brk--)
        {
          if (!std::isinf (st.at (brk, sys).details_.force_))
            {
              if (brk != end_brk)
                {
                  brk = st.at (brk, sys).prev_;
                  sys--;
                  warning (
                    _ ("cannot find line breaking that satisfies constraints"));
                  ret.push_back (space_line (brk, end_brk));
                }

              /* build up the good part of the solution */
              for (vsize cur_sys = sys; cur_sys != VPOS; cur_sys--)
                {
                  vsize prev_brk = st.at (brk, cur_sys).prev_;
                  assert (brk != VPOS);
                  ret.push_back (
                    space_line (prev_brk + start_brk, brk + start_brk));
                  brk = prev_brk;
                }
              std::reverse (ret.begin (), ret.end ());
              return ret;
            }
        }
    }
  /* if we get to here, just put everything on one line */
  if (sys_count > 0)
    {
      warning (_ ("cannot find line breaking that satisfies constraints"));
      ret.push_back (space_line (0, end_brk));
    }
  return ret;
}

vector<Column_x_positions>
Constrained_breaking::best_solution (vsize start, vsize end)
{
  vsize min_systems = min_system_count (start, end);
  vsize max_systems = max_system_count (start, end);
  Real best_demerits = infinity_f;
  vector<Column_x_positions> best_so_far;

  for (vsize i = min_systems; i <= max_systems; i++)
    {
      vsize brk = prepare_solution (start, end, i);
      Real dem = state_[start].at (brk, i - 1).demerits_;

      if (dem < best_demerits)
        {
          best_demerits = dem;
          best_so_far = solve (start, end, i);
        }
      else
        {
          vector<Column_x_positions> cur = solve (start, end, i);
          bool too_many_lines = true;

          for (vsize j = 0; j < cur.size (); j++)
            if (cur[j].force_ < 0)
              {
                too_many_lines = false;
                break;
              }
          if (too_many_lines)
            return best_so_far;
        }
    }
  if (best_so_far.size ())
    return best_so_far;
  return solve (start, end, max_systems);
}

std::vector<Line_details>
Constrained_breaking::line_details (vsize start, vsize end, vsize sys_count)
{
  vsize end_brk = prepare_solution (start, end, sys_count);
  Matrix<Constrained_break_node> const &st = state_[start];
  vector<Line_details> ret;

  /* This loop structure is C&Ped from solve(). */
  /* find the first solution that satisfies constraints */
  for (vsize sys = sys_count - 1; sys != VPOS; sys--)
    {
      for (vsize brk = end_brk; brk != VPOS; brk--)
        {
          if (!std::isinf (st.at (brk, sys).details_.force_))
            {
              if (brk != end_brk)
                {
                  /*
                    During initialize(), we only fill out a
                    Line_details for lines that are valid (ie. not too
                    long), otherwise line breaking becomes O(n^3).
                    In case sys_count is such that no valid solution
                    is found, we need to fill in the Line_details.
                  */
                  Line_details details;
                  brk = st.at (brk, sys).prev_;
                  sys--;
                  fill_line_details (&details, brk, end_brk);
                  ret.push_back (details);
                }

              /* build up the good part of the solution */
              for (vsize cur_sys = sys; cur_sys != VPOS; cur_sys--)
                {
                  vsize prev_brk = st.at (brk, cur_sys).prev_;
                  assert (brk != VPOS);
                  ret.push_back (st.at (brk, cur_sys).details_);
                  brk = prev_brk;
                }
              std::reverse (ret.begin (), ret.end ());
              return ret;
            }
        }
    }

  /* if we get to here, just put everything on one line */
  if (sys_count > 0)
    {
      Line_details details;
      fill_line_details (&details, 0, end_brk);
      ret.push_back (details);
    }
  return ret;
}

vsize
Constrained_breaking::min_system_count (vsize start, vsize end)
{
  vsize sys_count;
  vsize brk = prepare_solution (start, end, 1);
  vsize rank = breaks_.size () - starting_breakpoints_[start];
  Matrix<Constrained_break_node> const &st = state_[start];

  /* sys_count < rank : rank is the # of breakpoints, we can't have more systems */
  for (sys_count = 0; sys_count < rank; sys_count++)
    {
      if (sys_count >= valid_systems_)
        {
          resize (sys_count + 3);
        }
      if (!std::isinf (st.at (brk, sys_count).details_.force_))
        return sys_count + 1;
    }
  /* no possible breaks satisfy constraints */
  return 1;
}

vsize
Constrained_breaking::max_system_count (vsize start, vsize end) const
{
  vsize brk = (end >= start_.size ()) ? breaks_.size () - 1
                                      : starting_breakpoints_[end];
  return brk - starting_breakpoints_[start];
}

vsize
Constrained_breaking::prepare_solution (vsize start, vsize end, vsize sys_count)
{
  assert (start < start_.size () && (end == VPOS || end <= start_.size ()));
  assert (start < end);

  resize (sys_count);
  if (end == start_.size ())
    end = VPOS;

  vsize brk;
  brk = end == VPOS ? breaks_.size () - 1 : starting_breakpoints_[end];
  brk -= starting_breakpoints_[start];
  return brk;
}

Constrained_breaking::Constrained_breaking (Paper_score *ps)
{
  vector<vsize> start;
  start.push_back (0);
  initialize (ps, start);
}

Constrained_breaking::Constrained_breaking (Paper_score *ps,
                                            vector<vsize> const &start)
{
  initialize (ps, start);
}

static SCM
min_permission (SCM perm1, SCM perm2)
{
  if (scm_is_eq (perm1, ly_symbol2scm ("force")))
    return perm2;
  if (scm_is_eq (perm1, ly_symbol2scm ("allow"))
      && !scm_is_eq (perm2, ly_symbol2scm ("force")))
    return perm2;
  return SCM_EOL;
}

/* find the forces for all possible lines and cache ragged_ and ragged_right_
 */
void
Constrained_breaking::initialize (Paper_score *ps,
                                  vector<vsize> const &pagebreak_col_indices)
{
  valid_systems_ = systems_ = 0;
  pscore_ = ps;

  system_system_space_ = 0;
  system_markup_space_ = 0;
  system_system_padding_ = 0;
  system_system_min_distance_ = 0;
  score_system_padding_ = 0;
  score_system_min_distance_ = 0;
  score_markup_padding_ = 0;
  score_markup_min_distance_ = 0;

  if (!pscore_)
    {
      ragged_right_ = false;
      ragged_last_ = false;
      return;
    }

  ragged_right_
    = from_scm<bool> (pscore_->layout ()->c_variable ("ragged-right"));
  ragged_last_
    = from_scm<bool> (pscore_->layout ()->c_variable ("ragged-last"));

  Output_def *l = pscore_->layout ();

  SCM spacing_spec = l->c_variable ("system-system-spacing");
  SCM between_scores_spec = l->c_variable ("score-system-spacing");
  SCM title_spec = l->c_variable ("score-markup-spacing");
  SCM page_breaking_spacing_spec
    = l->c_variable ("page-breaking-system-system-spacing");

  Page_layout_problem::read_spacing_spec (spacing_spec, &system_system_space_,
                                          ly_symbol2scm ("basic-distance"));
  Page_layout_problem::read_spacing_spec (page_breaking_spacing_spec,
                                          &system_system_space_,
                                          ly_symbol2scm ("basic-distance"));
  Page_layout_problem::read_spacing_spec (title_spec, &system_markup_space_,
                                          ly_symbol2scm ("basic-distance"));

  Page_layout_problem::read_spacing_spec (spacing_spec, &system_system_padding_,
                                          ly_symbol2scm ("padding"));
  Page_layout_problem::read_spacing_spec (
    between_scores_spec, &score_system_padding_, ly_symbol2scm ("padding"));
  Page_layout_problem::read_spacing_spec (page_breaking_spacing_spec,
                                          &system_system_padding_,
                                          ly_symbol2scm ("padding"));
  Page_layout_problem::read_spacing_spec (title_spec, &score_markup_padding_,
                                          ly_symbol2scm ("padding"));

  Page_layout_problem::read_spacing_spec (between_scores_spec,
                                          &score_system_min_distance_,
                                          ly_symbol2scm ("minimum-distance"));
  Page_layout_problem::read_spacing_spec (spacing_spec,
                                          &system_system_min_distance_,
                                          ly_symbol2scm ("minimum-distance"));
  Page_layout_problem::read_spacing_spec (page_breaking_spacing_spec,
                                          &system_system_min_distance_,
                                          ly_symbol2scm ("minimum-distance"));
  Page_layout_problem::read_spacing_spec (title_spec,
                                          &score_markup_min_distance_,
                                          ly_symbol2scm ("minimum-distance"));

  Interval first_line = line_dimension_interval (pscore_->layout (), 0);
  Interval other_lines = line_dimension_interval (pscore_->layout (), 1);
  /* do all the rod/spring problems */
  breaks_ = pscore_->get_break_indices ();
  all_ = pscore_->root_system ()->used_columns ();
  lines_.resize (breaks_.size (), breaks_.size (), Line_details ());
  vector<Real> forces = get_line_forces (
    all_, other_lines.length (), other_lines.length () - first_line.length (),
    ragged_right_);
  for (vsize i = 0; i + 1 < breaks_.size (); i++)
    {
      for (vsize j = i + 1; j < breaks_.size (); j++)
        {
          bool last = j == breaks_.size () - 1;
          bool ragged = ragged_right_ || (last && ragged_last_);
          Line_details &line = lines_.at (j, i);

          line.force_ = forces[i * breaks_.size () + j];
          if (ragged && last && !std::isinf (line.force_))
            line.force_ = (line.force_ < 0 && j > i + 1) ? infinity_f : 0;
          if (std::isinf (line.force_))
            break;

          fill_line_details (&line, i, j);
        }
    }

  /* work out all the starting indices */
  start_.reserve (pagebreak_col_indices.size ());
  for (vsize pb_col : pagebreak_col_indices)
    {
      /* it would seem logical to require that pagebreak_col_indices
         is strictly increasing, but repeated entries can happen,
         eg. when starting a score with a \pageBreak
       */
      vsize j;
      for (j = 0; j + 1 < breaks_.size () && breaks_[j] < pb_col; j++)
        ;
      starting_breakpoints_.push_back (j);
      start_.push_back (breaks_[j]);
    }
  state_.resize (start_.size ());
}

/*
  Fills out all of the information contained in a Line_details,
  except for information about horizontal spacing.
*/
void
Constrained_breaking::fill_line_details (Line_details *const out, vsize start,
                                         vsize end)
{
  int start_rank = all_[breaks_[start]]->get_rank ();
  int end_rank = all_[breaks_[end]]->get_rank ();
  System *sys = pscore_->root_system ();
  Interval begin_of_line_extent
    = sys->begin_of_line_pure_height (start_rank, end_rank);
  Interval rest_of_line_extent
    = sys->rest_of_line_pure_height (start_rank, end_rank);
  bool last = (end == breaks_.size () - 1);

  Paper_column *c = all_[breaks_[end]];
  out->last_column_ = c;
  out->break_penalty_
    = from_scm<double> (get_property (c, "line-break-penalty"), 0);
  out->page_penalty_
    = from_scm<double> (get_property (c, "page-break-penalty"), 0);
  out->turn_penalty_
    = from_scm<double> (get_property (c, "page-turn-penalty"), 0);
  out->break_permission_ = get_property (c, "line-break-permission");
  out->page_permission_ = get_property (c, "page-break-permission");
  out->turn_permission_ = get_property (c, "page-turn-permission");

  /* turn permission should always be stricter than page permission
     and page permission should always be stricter than line permission */
  out->page_permission_
    = min_permission (out->break_permission_, out->page_permission_);
  out->turn_permission_
    = min_permission (out->page_permission_, out->turn_permission_);

  begin_of_line_extent = (begin_of_line_extent.is_empty ()
                          || std::isnan (begin_of_line_extent[LEFT])
                          || std::isnan (begin_of_line_extent[RIGHT]))
                           ? Interval (0, 0)
                           : begin_of_line_extent;
  rest_of_line_extent
    = (rest_of_line_extent.is_empty () || std::isnan (rest_of_line_extent[LEFT])
       || std::isnan (rest_of_line_extent[RIGHT]))
        ? Interval (0, 0)
        : rest_of_line_extent;
  out->shape_ = Line_shape (begin_of_line_extent, rest_of_line_extent);
  out->padding_ = last ? score_system_padding_ : system_system_padding_;
  out->title_padding_ = score_markup_padding_;
  out->min_distance_
    = last ? score_system_min_distance_ : system_system_min_distance_;
  out->title_min_distance_ = score_markup_min_distance_;
  out->space_ = system_system_space_;
  out->title_space_ = system_markup_space_;
  out->inverse_hooke_ = out->full_height () + system_system_space_;

  out->footnote_heights_
    = sys->get_footnote_heights_in_range (start_rank, end_rank);
  out->in_note_heights_
    = sys->get_in_note_heights_in_range (start_rank, end_rank);

  out->refpoint_extent_ = sys->pure_refpoint_extent (start_rank, end_rank);
  if (out->refpoint_extent_.is_empty ())
    out->refpoint_extent_ = Interval (0, 0);
}

Real
Constrained_breaking::combine_demerits (Real force, Real prev_force)
{
  if (ragged_right_)
    return force * force;

  return force * force + (prev_force - force) * (prev_force - force);
}

Line_details::Line_details (Prob *pb, Output_def *paper)
{
  SCM spec = paper->c_variable ("markup-system-spacing");
  SCM title_spec = paper->c_variable ("markup-markup-spacing");
  padding_ = 0;
  title_padding_ = 0;
  min_distance_ = 0;
  title_min_distance_ = 0;
  space_ = 0;
  title_space_ = 0;
  Page_layout_problem::read_spacing_spec (spec, &space_,
                                          ly_symbol2scm ("basic-distance"));
  Page_layout_problem::read_spacing_spec (title_spec, &title_space_,
                                          ly_symbol2scm ("basic-distance"));
  Page_layout_problem::read_spacing_spec (spec, &padding_,
                                          ly_symbol2scm ("padding"));
  Page_layout_problem::read_spacing_spec (title_spec, &title_padding_,
                                          ly_symbol2scm ("padding"));
  Page_layout_problem::read_spacing_spec (spec, &min_distance_,
                                          ly_symbol2scm ("minimum-distance"));
  Page_layout_problem::read_spacing_spec (title_spec, &title_min_distance_,
                                          ly_symbol2scm ("minimum-distance"));

  SCM footnotes = get_property (pb, "footnotes");

  if (scm_is_pair (footnotes))
    for (SCM s = footnotes; scm_is_pair (s); s = scm_cdr (s))
      {
        auto *sten = unsmob<const Stencil> (scm_caddar (s));
        if (!sten)
          {
            programming_error ("expecting stencil, got empty pointer");
            continue;
          }
        footnote_heights_.push_back (sten->extent (Y_AXIS).length ());
      }

  last_column_ = 0;
  force_ = 0;
  auto *st = unsmob<const Stencil> (get_property (pb, "stencil"));
  Interval stencil_extent
    = st->is_empty (Y_AXIS) ? Interval (0, 0) : st->extent (Y_AXIS);
  shape_ = Line_shape (stencil_extent,
                       stencil_extent); // pretend it goes all the way across
  tallness_ = 0;
  bottom_padding_ = 0;
  inverse_hooke_ = 1.0;
  break_permission_ = ly_symbol2scm ("allow");
  page_permission_ = get_property (pb, "page-break-permission");
  turn_permission_ = get_property (pb, "page-turn-permission");
  break_penalty_ = 0;
  page_penalty_ = from_scm<double> (get_property (pb, "page-break-penalty"), 0);
  turn_penalty_ = from_scm<double> (get_property (pb, "page-turn-penalty"), 0);
  title_ = from_scm<bool> (get_property (pb, "is-title"));
  compressed_lines_count_ = 1;
  compressed_nontitle_lines_count_ = title_ ? 0 : 1;
  SCM last_scm = get_property (pb, "last-markup-line");
  last_markup_line_ = from_scm<bool> (last_scm);
  SCM first_scm = get_property (pb, "first-markup-line");
  first_markup_line_ = from_scm<bool> (first_scm);
  tight_spacing_ = from_scm<bool> (get_property (pb, "tight-spacing"));
  refpoint_extent_ = Interval (0, 0);
}

Real
Line_details::full_height () const
{
  Interval ret;
  ret.unite (shape_.begin_);
  ret.unite (shape_.rest_);
  return ret.length ();
}

Real
Line_details::tallness () const
{
  return tallness_;
}

Real
Line_details::spring_length (Line_details const &next_line) const
{
  // space_ measures the spring which goes from the bottom refpoint
  // of this to the top refpoint of next_line. We want to return
  // the stretchable space between the bottom of this's extent to
  // the top of next_line's extent.
  Real refpoint_dist
    = tallness_ + refpoint_extent_[DOWN] - next_line.refpoint_extent_[UP];
  Real space = next_line.title_ ? title_space_ : space_;
  return std::max (0.0, space - refpoint_dist);
}

Line_shape::Line_shape (Interval begin, Interval rest)
{
  begin_ = begin;
  rest_ = rest;
}

Line_shape
Line_shape::piggyback (Line_shape mount, Real padding) const
{
  Real elevation
    = std::max (begin_[UP] - mount.begin_[DOWN], rest_[UP] - mount.rest_[DOWN]);
  Interval begin
    = Interval (begin_[DOWN], elevation + mount.begin_[UP] + padding);
  Interval rest = Interval (rest_[DOWN], elevation + mount.rest_[UP] + padding);
  return Line_shape (begin, rest);
}
