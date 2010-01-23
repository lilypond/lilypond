/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>

#include "column-x-positions.hh"
#include "dimensions.hh"
#include "international.hh"
#include "libc-extension.hh"	// isinf
#include "paper-column.hh"
#include "simple-spacer.hh"
#include "spaceable-grob.hh"
#include "spring.hh"
#include "warn.hh"

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

Simple_spacer::Simple_spacer ()
{
  line_len_ = 0.0;
  force_ = 0.0;
  fits_ = true;
  ragged_ = true;
}

Real
Simple_spacer::force () const
{
  return force_;
}

bool
Simple_spacer::fits () const
{
  return fits_;
}

Real
Simple_spacer::rod_force (int l, int r, Real dist)
{
  Real d = range_ideal_len (l, r);
  Real c = range_stiffness (l, r, dist > d);
  Real block_stretch = dist - d;

  if (isinf (c) && block_stretch == 0) /* take care of the 0*infinity_f case */
    return 0;
  return c * block_stretch;
}

void
Simple_spacer::add_rod (int l, int r, Real dist)
{
  if (isinf (dist) || isnan (dist))
    {
      programming_error ("ignoring weird minimum distance");
      return;
    }

  Real block_force = rod_force (l, r, dist);

  if (isinf (block_force))
    {
      Real spring_dist = range_ideal_len (l, r);
      if (spring_dist < dist)
	for (int i = l; i < r; i++)
	  {
	    if (spring_dist)
	      springs_[i].set_distance (springs_[i].distance () * dist / spring_dist);
	    else
	      springs_[i].set_distance (dist / (r - l));
	  }

      return;
    }
  force_ = max (force_, block_force);
  for (int i = l; i < r; i++)
    springs_[i].set_blocking_force (max (block_force, springs_[i].blocking_force ()));
}

Real
Simple_spacer::range_ideal_len (int l, int r) const
{
  Real d = 0.;
  for (int i = l; i < r; i++)
    d += springs_[i].distance ();
  return d;
}

Real
Simple_spacer::range_stiffness (int l, int r, bool stretch) const
{
  Real den = 0.0;
  for (int i = l; i < r; i++)
    den += stretch ? springs_[i].inverse_stretch_strength ()
      : springs_[i].inverse_compress_strength ();

  return 1 / den;
}

Real
Simple_spacer::configuration_length (Real force) const
{
  Real l = 0.;
  for (vsize i = 0; i < springs_.size (); i++)
    l += springs_[i].length (force);

  return l;
}

void
Simple_spacer::solve (Real line_len, bool ragged)
{
  Real conf = configuration_length (force_);

  ragged_ = ragged;
  line_len_ = line_len;
  if (conf < line_len_)
    force_ = expand_line ();
  else if (conf > line_len_)
    force_ = compress_line ();

  if (ragged && force_ < 0)
    fits_ = false;
}

Real
Simple_spacer::expand_line ()
{
  double inv_hooke = 0;
  double cur_len = configuration_length (force_);

  fits_ = true;
  for (vsize i=0; i < springs_.size (); i++)
    inv_hooke += springs_[i].inverse_stretch_strength ();

  if (inv_hooke == 0.0) /* avoid division by zero. If springs are infinitely stiff */
    return 0.0;         /* anyway, then it makes no difference what the force is */

  assert (cur_len <= line_len_);
  return (line_len_ - cur_len) / inv_hooke + force_;
}

Real
Simple_spacer::compress_line ()
{
  double inv_hooke = 0;
  double cur_len = configuration_length (force_);
  double cur_force = force_;
  bool compressed = false;

  /* just because we are in compress_line () doesn't mean that the line
     will actually be compressed (as in, a negative force) because
     we start out with a stretched line. Here, we check whether we
     will be compressed or stretched (so we know which spring constant to use) */
  if (configuration_length (0.0) > line_len_)
    {
      cur_force = 0.0;
      cur_len = configuration_length (0.0);
      compressed = true;
    }

  fits_ = true;
  for (vsize i=0; i < springs_.size (); i++)
    inv_hooke += compressed
      ? springs_[i].inverse_compress_strength ()
      : springs_[i].inverse_stretch_strength ();

  assert (line_len_ <= cur_len);

  vector<Spring> sorted_springs = springs_;
  sort (sorted_springs.begin (), sorted_springs.end (), greater<Spring> ());

  for (vsize i = 0; i < sorted_springs.size (); i++)
    {
      Spring sp = sorted_springs[i];

      if (sp.blocking_force () > cur_force)
	continue;

      if (isinf (sp.blocking_force ()))
	break;

      double block_dist = (cur_force - sp.blocking_force ()) * inv_hooke;
      if (cur_len - block_dist < line_len_)
	{
	  cur_force += (line_len_ - cur_len) / inv_hooke;
	  cur_len = line_len_;

	  /*
	    Paranoia check.
	  */
	  assert (fabs (configuration_length (cur_force) - cur_len) < 1e-6);
	  return cur_force;
	}
      
      cur_len -= block_dist;
      inv_hooke -= compressed ? sp.inverse_compress_strength () : sp.inverse_stretch_strength ();
      cur_force = sp.blocking_force ();
    }

  fits_ = false;
  return cur_force;
}

void
Simple_spacer::add_spring (Spring const &sp)
{
  force_ = max (force_, sp.blocking_force ());
  springs_.push_back (sp);
}

vector<Real>
Simple_spacer::spring_positions () const
{
  vector<Real> ret;
  ret.push_back (0.);

  for (vsize i = 0; i < springs_.size (); i++)
    ret.push_back (ret.back () + springs_[i].length (ragged_ && force_ > 0 ? 0.0 : force_));
  return ret;
}

Real
Simple_spacer::force_penalty (bool ragged) const
{
  /* If we are ragged-right, we don't want to penalise according to the force,
     but according to the amount of whitespace that is present after the end
     of the line. */
  if (ragged)
    return max (0.0, line_len_ - configuration_length (0.0));

  /* Use a convex compression penalty. */
  Real f = force_;
  return f - (f < 0 ? f*f*f*f*2 : 0);
}

/****************************************************************/

struct Rod_description
{
  vsize r_;
  Real dist_;

  bool operator< (const Rod_description r)
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
  return (scm_is_pair (g->get_object ("between-cols")));
}

static Grob*
maybe_find_prebroken_piece (Grob *g, Direction d)
{
  Grob *ret = dynamic_cast<Item*> (g)->find_prebroken_piece (d);
  if (ret)
    return ret;
  return g;
}

static Grob*
next_spaceable_column (vector<Grob*> const &list, vsize starting)
{
  for (vsize i = starting+1; i < list.size (); i++)
    if (!is_loose (list[i]))
      return list[i];
  return 0;
}

static Column_description
get_column_description (vector<Grob*> const &cols, vsize col_index, bool line_starter)
{
  Grob *col = cols[col_index];
  if (line_starter)
    col = maybe_find_prebroken_piece (col, RIGHT);

  Column_description description;
  Grob *next_col = next_spaceable_column (cols, col_index);
  if (next_col)
    description.spring_ = Spaceable_grob::get_spring (col, next_col);

  Grob *end_col = dynamic_cast<Item*> (cols[col_index+1])->find_prebroken_piece (LEFT);
  if (end_col)
    description.end_spring_ = Spaceable_grob::get_spring (col, end_col);

  for (SCM s = Spaceable_grob::get_minimum_distances (col);
       scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *other = unsmob_grob (scm_caar (s));
      vsize j = binary_search (cols, other, Paper_column::less_than, col_index);
      if (j != VPOS)
	{
	  if (cols[j] == other)
	    description.rods_.push_back (Rod_description (j, scm_to_double (scm_cdar (s))));
	  else /* it must end at the LEFT prebroken_piece */
	    description.end_rods_.push_back (Rod_description (j, scm_to_double (scm_cdar (s))));
	}
    }
  
  if (!line_starter && to_boolean (col->get_property ("keep-inside-line")))
    description.keep_inside_line_ = col->extent (col, X_AXIS);

  description.break_permission_ = col->get_property ("line-break-permission");
  return description;
}

vector<Real>
get_line_forces (vector<Grob*> const &columns,
		 Real line_len, Real indent, bool ragged)
{
  vector<vsize> breaks;
  vector<Real> force;
  vector<Grob*> non_loose;
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

      for (vsize c = b+1; c < breaks.size (); c++)
	{
	  vsize end = breaks[c];
	  Simple_spacer spacer;

	  for (vsize i = breaks[b]; i < end - 1; i++)
	    spacer.add_spring (cols[i].spring_);
	  spacer.add_spring (cols[end-1].end_spring_);


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
	  spacer.solve ((b == 0) ? line_len - indent : line_len, ragged);
	  force[b * breaks.size () + c] = spacer.force_penalty (ragged);

	  if (!spacer.fits ())
	    {
	      if (c == b + 1)
		force[b * breaks.size () + c] = -200000;
	      else
		force[b * breaks.size () + c] = infinity_f;
	      break;
	    }
	  if (end < cols.size () && cols[end].break_permission_ == force_break)
	    break;
	}
    }
  return force;
}

Column_x_positions
get_line_configuration (vector<Grob*> const &columns,
			Real line_len,
			Real indent,
			bool ragged)
{
  vector<Column_description> cols;
  Simple_spacer spacer;
  Column_x_positions ret;

  ret.cols_.push_back (dynamic_cast<Item*> (columns[0])->find_prebroken_piece (RIGHT));
  for (vsize i = 1; i + 1 < columns.size (); i++)
    {
      if (is_loose (columns[i]))
	ret.loose_cols_.push_back (columns[i]);
      else
	ret.cols_.push_back (columns[i]);
    }
  ret.cols_.push_back (dynamic_cast<Item*> (columns.back ())->find_prebroken_piece (LEFT));

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

  spacer.solve (line_len, ragged);
  ret.force_ = spacer.force_penalty (ragged);

  ret.config_ = spacer.spring_positions ();
  for (vsize i = 0; i < ret.config_.size (); i++)
    ret.config_[i] += indent;

  ret.satisfies_constraints_ = spacer.fits ();

  /*
    Check if breaking constraints are met.
  */
  for (vsize i = 1; i + 1 < ret.cols_.size (); i++)
    {
      SCM p = ret.cols_[i]->get_property ("line-break-permission");
      if (p == ly_symbol2scm ("force"))
	ret.satisfies_constraints_ = false;
    }

  return ret;
}

