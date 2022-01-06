/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  TODO:
  - add support for different stretch/shrink constants?

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

#include "column-x-positions.hh"
#include "dimensions.hh"
#include "international.hh"
#include "paper-column.hh"
#include "simple-spacer.hh"
#include "spaceable-grob.hh"
#include "spring.hh"
#include "warn.hh"

#include <algorithm>
#include <cstdio>
#include <vector>

using std::vector;

/*
  A simple spacing constraint solver. The approach:

  Stretch the line uniformly until none of the constraints (rods)
  block.  It then is very wide.

  Compress until the next constraint blocks,

  Mark the springs over the constrained part to be non-active.

  Repeat with the smaller set of non-active constraints, until all
  constraints blocked, or until the line is as short as desired.

  This is much simpler, and much much faster than full scale
  Constrained QP. On the other hand, a situation like this will not
  be typeset as dense as possible, because

  c4                   c4           c4                  c4
  veryveryverylongsyllable2         veryveryverylongsyllable2
  " "4                 veryveryverylongsyllable2        syllable4


  can be further compressed to


  c4    c4                        c4   c4
  veryveryverylongsyllable2       veryveryverylongsyllable2
  " "4  veryveryverylongsyllable2      syllable4


  Perhaps this is not a bad thing, because the 1st looks better anyway.  */

/*
  positive force = expanding, negative force = compressing.
*/

Simple_spacer::Simple_spacer () {}

/*
   Heuristically try to calculate the force that will get us as close to the
   rod distance as possible without being less than it.
*/
Real
Simple_spacer::heuristic_rod_force (vsize l, vsize r, Real dist) const
{
  Real ideal_length = range_ideal_len (l, r);
  Real stiffness = range_stiffness (l, r, dist > ideal_length);
  Real block_stretch = dist - ideal_length;

  if (std::isinf (stiffness)) // nothing we can do here
    return stiffness;

  Real previous_force = stiffness * block_stretch;
  Real previous_length = range_len (l, r, previous_force);
  Real overcompensation = previous_length - dist;

  if (! (overcompensation > 1e-6))
    return previous_force;

  Real shrink_value = overcompensation / static_cast<Real> (r - l);
  Real current_force = previous_force;
  Real current_length = previous_length;
  while (overcompensation > 1e-6 && shrink_value > 1e-6)
    {
      previous_force = current_force;
      previous_length = current_length;

      current_force -= shrink_value;
      current_length = range_len (l, r, current_force);
      if (current_length < dist) // We've gone too far
        {
          // Go back one step and try again with smaller shrink values
          current_force = previous_force;
          current_length = previous_length;
          shrink_value /= 2;
        }

      // Stop trying when we can't make it any smaller
      if (! (previous_length - current_length > 1e-6))
        break;
    }
  return current_force;
}

void
Simple_spacer::add_rod (vsize l, vsize r, Real dist)
{
  if (!std::isfinite (dist))
    {
      programming_error ("ignoring weird minimum distance");
      return;
    }

  Real block_force = heuristic_rod_force (l, r, dist);

  if (std::isinf (block_force))
    {
      Real spring_dist = range_ideal_len (l, r);
      if (spring_dist < dist)
        {
          Real factor = spring_dist > 0.0 ? dist / spring_dist
                                          : dist / static_cast<Real> (r - l);

          for (vsize i = l; i < r; i++)
            {
              if (spring_dist > 0)
                springs_[i].set_ideal_distance (springs_[i].ideal_distance ()
                                                * factor);
              else
                springs_[i].set_ideal_distance (factor);
            }
        }
      return;
    }
  for (vsize i = l; i < r; i++)
    springs_[i].set_blocking_force (std::max (block_force, springs_[i].blocking_force ()));
}

Real
Simple_spacer::range_len (vsize l, vsize r, Real force) const
{
  Real d = 0.;
  for (vsize i = l; i < r; i++)
    d += springs_[i].length (force);
  return d;
}

Real
Simple_spacer::range_ideal_len (vsize l, vsize r) const
{
  Real d = 0.;
  for (vsize i = l; i < r; i++)
    d += springs_[i].ideal_distance ();
  return d;
}

Real
Simple_spacer::range_stiffness (vsize l, vsize r, bool stretch) const
{
  Real den = 0.0;
  for (vsize i = l; i < r; i++)
    den += stretch ? springs_[i].inverse_stretch_strength ()
           : springs_[i].inverse_compress_strength ();

  return 1 / den;
}

Real
Simple_spacer::configuration_length (Real force) const
{
  return range_len (0, springs_.size (), force);
}

Real
Simple_spacer::range_max_block_force (vsize l, vsize r) const
{
  Real result = 0.0;
  for (vsize i = l; i < r; i++)
    result = std::max (result, springs_[i].blocking_force ());

  return result;
}

Simple_spacer::Solution
Simple_spacer::solve (Real line_len, bool ragged) const
{
  Real max_block_force = range_max_block_force (0, springs_.size ());
  Real max_block_force_len = configuration_length (max_block_force);

  Solution sol;
  if (max_block_force_len < line_len)
    sol = expand_line (line_len, max_block_force_len, max_block_force);
  else if (max_block_force_len > line_len)
    sol = compress_line (line_len, max_block_force_len, max_block_force);
  else
    {
      sol.force_ = max_block_force;
      sol.fits_ = true;
    }

  if (ragged && sol.force_ < 0)
    sol.fits_ = false;

  return sol;
}

Simple_spacer::Solution
Simple_spacer::expand_line (Real line_len, Real max_block_force_len,
                            Real max_block_force) const
{
  double inv_hooke = 0;

  for (vsize i = 0; i < springs_.size (); i++)
    inv_hooke += springs_[i].inverse_stretch_strength ();

  if (inv_hooke == 0.0) /* avoid division by zero. If springs are infinitely stiff */
    inv_hooke = 1e-6;   /* then report a very large stretching force */

  Solution sol;
  sol.force_ = (line_len - max_block_force_len) / inv_hooke + max_block_force;
  sol.fits_ = true;
  return sol;
}

static bool
spring_pointer_greater (Spring const *const a, Spring const *const b)
{
  return *a > *b;
}

Simple_spacer::Solution
Simple_spacer::compress_line (Real line_len, Real max_block_force_len,
                              Real max_block_force) const
{
  /* just because we are in compress_line () doesn't mean that the line
     will actually be compressed (as in, a negative force) because
     we start out with a stretched line. Here, we check whether we
     will be compressed or stretched (so we know which spring constant to use) */
  double neutral_length = configuration_length (0.0);
  bool compressed = (neutral_length > line_len);

  Simple_spacer::Solution cur;
  cur.force_ = compressed ? 0.0 : max_block_force;
  cur.fits_ = true;
  Real cur_len = compressed ? neutral_length : max_block_force_len;

  vector<const Spring *> sorted_springs;
  sorted_springs.reserve (springs_.size ());
  for (vsize i = 0; i < springs_.size (); i++)
    sorted_springs.push_back (&springs_[i]);

  sort (sorted_springs.begin (), sorted_springs.end (), spring_pointer_greater);

  /* inv_hooke is the total flexibility of currently-active springs */
  double inv_hooke = 0;
  vsize i = sorted_springs.size ();
  for (; i && sorted_springs[i - 1]->blocking_force () < cur.force_; i--)
    inv_hooke += compressed
                   ? sorted_springs[i - 1]->inverse_compress_strength ()
                   : sorted_springs[i - 1]->inverse_stretch_strength ();
  /* i now indexes the first active spring, so */
  for (; i < sorted_springs.size (); i++)
    {
      const Spring *sp = sorted_springs[i];

      if (std::isinf (sp->blocking_force ()))
        break;

      double block_dist = (cur.force_ - sp->blocking_force ()) * inv_hooke;
      if (cur_len - block_dist < line_len)
        {
          cur.force_ += (line_len - cur_len) / inv_hooke;
          cur_len = line_len;
          return cur;
        }

      cur_len -= block_dist;
      inv_hooke -= compressed ? sp->inverse_compress_strength ()
                              : sp->inverse_stretch_strength ();
      cur.force_ = sp->blocking_force ();
    }

  cur.fits_ = false;
  return cur;
}

void
Simple_spacer::add_spring (Spring const &sp)
{
  springs_.push_back (sp);
}

vector<Real>
Simple_spacer::spring_positions (Real force, bool ragged) const
{
  vector<Real> ret;
  ret.push_back (0.);

  for (vsize i = 0; i < springs_.size (); i++)
    ret.push_back (ret.back ()
                   + springs_[i].length (ragged && force > 0 ? 0.0 : force));

  return ret;
}

Real
Simple_spacer::force_penalty (Real line_len, Real force, bool ragged) const
{
  /* If we are ragged-right, we don't want to penalise according to the force,
     but according to the amount of whitespace that is present after the end
     of the line. */
  if (ragged)
    return std::max (0.0, line_len - configuration_length (0.0));

  /* Use a convex compression penalty. */
  Real f = force;
  return f - (f < 0 ? f * f * f * f * 2 : 0);
}

/****************************************************************/

struct Rod_description
{
  vsize r_;
  Real dist_;

  bool operator < (const Rod_description r)
  {
    return r_ < r.r_;
  }

  Rod_description ()
  {
    r_ = 0;
    dist_ = 0;
  }

  Rod_description (vsize r, Real d)
  {
    r_ = r;
    dist_ = d;
  }
};

struct Column_description
{
  vector<Rod_description> rods_;
  vector<Rod_description> end_rods_;   /* use these if they end at the last column of the line */
  Spring spring_;
  Spring end_spring_;

  SCM break_permission_;
  Interval keep_inside_line_;

  Column_description ()
  {
    break_permission_ = SCM_EOL;
  }
};

static bool
is_loose (Grob *g)
{
  return (scm_is_pair (get_object (g, "between-cols")));
}

static Paper_column *
maybe_find_prebroken_piece (Paper_column *col, Direction d)
{
  if (Paper_column *ret = col->find_prebroken_piece (d))
    return ret;
  return col;
}

static Paper_column *
next_spaceable_column (vector<Paper_column *> const &list, vsize starting)
{
  for (vsize i = starting + 1; i < list.size (); i++)
    if (!is_loose (list[i]))
      return list[i];
  return 0;
}

static Column_description
get_column_description (vector<Paper_column *> const &cols, vsize col_index, bool line_starter)
{
  Paper_column *col = cols[col_index];
  if (line_starter)
    col = maybe_find_prebroken_piece (col, RIGHT);

  Column_description description;
  Paper_column *next_col = next_spaceable_column (cols, col_index);
  if (next_col)
    description.spring_ = Spaceable_grob::get_spring (col, next_col);

  if (col_index + 1 < cols.size ())
    {
      Paper_column *end_col = cols[col_index + 1]->find_prebroken_piece (LEFT);
      if (end_col)
        description.end_spring_ = Spaceable_grob::get_spring (col, end_col);
    }

  for (SCM s = Spaceable_grob::get_minimum_distances (col);
       scm_is_pair (s); s = scm_cdr (s))
    {
      if (Paper_column *other = unsmob<Paper_column> (scm_caar (s)))
        {
          auto j = std::lower_bound (cols.begin () + col_index,
                                     cols.end (), other,
                                     Paper_column::rank_less);
          if (j != cols.end () && (*j)->get_rank () == other->get_rank ())
            {
              Real dist = scm_to_double (scm_cdar (s));
              vsize idx = j - cols.begin ();
              if (*j == other)
                description.rods_.push_back (Rod_description (idx, dist));
              else /* it must end at the LEFT prebroken_piece */
                /* see Spanner::set_spacing_rods for more comments on how
                   to deal with situations where  we don't know if we're
                   ending yet on the left prebroken piece */
                description.end_rods_.push_back (Rod_description (idx, dist));
            }
        }
      else
        {
          programming_error ("minimum-distances holds an object that"
                             " is not a paper column");
        }
    }

  if (!line_starter && from_scm<bool> (get_property (col, "keep-inside-line")))
    description.keep_inside_line_ = col->extent (col, X_AXIS);

  description.break_permission_ = get_property (col, "line-break-permission");
  return description;
}

/* returns a vector of dimensions breaks.size () * breaks.size ()

   Compute the forces for all (start, end) combinations where
   start/end are breakable columns. The force for (start, end) is in
   (start_break_index * breaks.size + end_break_index)

   If the combination doesn't fit, use infinity as force.
 */
vector<Real>
get_line_forces (vector<Paper_column *> const &columns,
                 Real line_len, Real indent, bool ragged)
{
  vector<vsize> breaks;
  vector<Real> force;
  vector<Paper_column *> non_loose;
  vector<Column_description> cols;
  SCM force_break = ly_symbol2scm ("force");

  for (vsize i = 0; i < columns.size (); i++)
    if (!is_loose (columns[i]) || Paper_column::is_breakable (columns[i]))
      non_loose.push_back (columns[i]);

  breaks.clear ();
  breaks.push_back (0);
  cols.push_back (Column_description ());
  for (vsize i = 1; i + 1 < non_loose.size (); i++)
    {
      if (Paper_column::is_breakable (non_loose[i]))
        breaks.push_back (cols.size ());

      cols.push_back (get_column_description (non_loose, i, false));
    }
  breaks.push_back (cols.size ());
  force.resize (breaks.size () * breaks.size (), infinity_f);

  for (vsize b = 0; b + 1 < breaks.size (); b++)
    {
      cols[breaks[b]] = get_column_description (non_loose, breaks[b], true);
      vsize st = breaks[b];

      for (vsize c = b + 1; c < breaks.size (); c++)
        {
          vsize end = breaks[c];
          Simple_spacer spacer;

          for (vsize i = breaks[b]; i < end - 1; i++)
            spacer.add_spring (cols[i].spring_);
          spacer.add_spring (cols[end - 1].end_spring_);

          for (vsize i = breaks[b]; i < end; i++)
            {
              for (vsize r = 0; r < cols[i].rods_.size (); r++)
                if (cols[i].rods_[r].r_ < end)
                  spacer.add_rod (i - st, cols[i].rods_[r].r_ - st, cols[i].rods_[r].dist_);
              for (vsize r = 0; r < cols[i].end_rods_.size (); r++)
                if (cols[i].end_rods_[r].r_ == end)
                  spacer.add_rod (i - st, end - st, cols[i].end_rods_[r].dist_);
              if (!cols[i].keep_inside_line_.is_empty ())
                {
                  spacer.add_rod (i - st, end - st, cols[i].keep_inside_line_[RIGHT]);
                  spacer.add_rod (0, i - st, -cols[i].keep_inside_line_[LEFT]);
                }
            }
          Simple_spacer::Solution sol
            = spacer.solve ((b == 0) ? line_len - indent : line_len, ragged);
          force[b * breaks.size () + c]
            = spacer.force_penalty (line_len, sol.force_, ragged);

          if (!sol.fits_)
            {
              if (c == b + 1)
                force[b * breaks.size () + c] = -200000;
              else
                force[b * breaks.size () + c] = infinity_f;
              break;
            }
          if (end < cols.size () && scm_is_eq (cols[end].break_permission_, force_break))
            break;
        }
    }
  return force;
}

Column_x_positions
get_line_configuration (vector<Paper_column *> const &columns,
                        Real line_len,
                        Real indent,
                        bool ragged)
{
  vector<Column_description> cols;
  Simple_spacer spacer;
  Column_x_positions ret;

  ret.cols_.push_back (columns[0]->find_prebroken_piece (RIGHT));
  for (vsize i = 1; i + 1 < columns.size (); i++)
    {
      if (is_loose (columns[i]))
        ret.loose_cols_.push_back (columns[i]);
      else
        ret.cols_.push_back (columns[i]);
    }
  ret.cols_.push_back (columns.back ()->find_prebroken_piece (LEFT));

  /* since we've already put our line-ending column in the column list, we can ignore
     the end_XXX_ fields of our column_description */
  for (vsize i = 0; i + 1 < ret.cols_.size (); i++)
    {
      cols.push_back (get_column_description (ret.cols_, i, i == 0));
      spacer.add_spring (cols[i].spring_);
    }
  for (vsize i = 0; i < cols.size (); i++)
    {
      for (vsize r = 0; r < cols[i].rods_.size (); r++)
        spacer.add_rod (i, cols[i].rods_[r].r_, cols[i].rods_[r].dist_);

      if (!cols[i].keep_inside_line_.is_empty ())
        {
          spacer.add_rod (i, cols.size (), cols[i].keep_inside_line_[RIGHT]);
          spacer.add_rod (0, i, -cols[i].keep_inside_line_[LEFT]);
        }
    }

  Simple_spacer::Solution sol = spacer.solve (line_len, ragged);
  ret.force_ = spacer.force_penalty (line_len, sol.force_, ragged);
  ret.config_ = spacer.spring_positions (sol.force_, ragged);
  for (vsize i = 0; i < ret.config_.size (); i++)
    ret.config_[i] += indent;

  ret.satisfies_constraints_ = sol.fits_;

  /*
    Check if breaking constraints are met.
  */
  for (vsize i = 1; i + 1 < ret.cols_.size (); i++)
    {
      SCM p = get_property (ret.cols_[i], "line-break-permission");
      if (scm_is_eq (p, ly_symbol2scm ("force")))
        ret.satisfies_constraints_ = false;
    }

  return ret;
}
