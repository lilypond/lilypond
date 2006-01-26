/*
  simple-spacer.cc -- implement Simple_spacer

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

  TODO:
  - add support for different stretch/shrink constants?
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
  /*
    Give an extra penalty for compression. Needed to avoid compressing
    tightly spaced lines.
  */
  active_count_ = 0;
  force_ = 0.;
  indent_ = 0.0;
  default_space_ = 20 PT;
}

void
Simple_spacer::add_rod (int l, int r, Real dist)
{
  if (isinf (dist) || isnan (dist))
    {
      programming_error ("ignoring weird minimum distance");
      return;
    }

  Real c = range_stiffness (l, r);
  if (isinf (c))
    {
      /*
	If a spring is fixed, we have to do something here:
	we let the rod override the spring.
      */
      Real total_dist = 0.;
      for (int i = l; i < r; i++)
	total_dist += springs_[i].ideal_;

      if (total_dist < dist)
	for (int i = l; i < r; i++)
	  springs_[i].ideal_ *= dist / total_dist;

      return;
    }

  Real d = range_ideal_len (l, r);
  Real block_stretch = dist - d;

  Real block_force = c * block_stretch;
  force_ = max (force_, block_force);

  for (int i = l; i < r; i++)
    springs_[i].block_force_ = max (block_force, springs_[i].block_force_);
}

Real
Simple_spacer::range_ideal_len (int l, int r) const
{
  Real d = 0.;
  for (int i = l; i < r; i++)
    d += springs_[i].ideal_;
  return d;
}

Real
Simple_spacer::range_stiffness (int l, int r) const
{
  Real den = 0.0;
  for (int i = l; i < r; i++)
    {
      if (springs_[i].is_active_)
	den += 1 * springs_[i].inverse_hooke_;
    }

  return 1 / den;
}

Real
Simple_spacer::active_blocking_force () const
{
  Real bf = -infinity_f;
  for (int i = 0; i < springs_.size (); i++)
    if (springs_[i].is_active_)
      bf = max (bf, springs_[i].block_force_);
  return bf;
}

Real
Simple_spacer::active_springs_stiffness () const
{
  Real stiff = range_stiffness (0, springs_.size ());
  if (isinf (stiff))
    {
      /*
	all springs are inactive. Take the stiffness of the
	latest spring to block.
      */

      Real max_block_force = -infinity_f;
      int max_i = -1;
      for (int i = 0; i < springs_.size (); i++)
	{
	  if (springs_[i].block_force_ > max_block_force)
	    {
	      max_i = i;
	      max_block_force = springs_[i].block_force_;
	    }
	}

      stiff = 1/springs_[max_i].inverse_hooke_;
    }
  return stiff;
}

void
Simple_spacer::set_active_states ()
{
  /* float comparison is safe, since force is only copied.  */
  for (int i = 0; i < springs_.size (); i++)
    if (springs_[i].is_active_
	&& springs_[i].block_force_ >= force_)
      {
	springs_[i].is_active_ = false;
	active_count_--;
      }
}

Real
Simple_spacer::configuration_length () const
{
  Real l = 0.;
  for (int i = 0; i < springs_.size (); i++)
    l += springs_[i].length (force_);

  return l;
}

bool
Simple_spacer::is_active () const
{
  return active_count_;
}

void
Simple_spacer::my_solve_linelen ()
{
  while (is_active ())
    {
      force_ = active_blocking_force ();
      Real conf = configuration_length ();

      if (conf < line_len_)
	{
	  force_ += (line_len_ - conf) * active_springs_stiffness ();
	  break;
	}
      else
	set_active_states ();
    }
}

void
Simple_spacer::my_solve_natural_len ()
{
  Real line_len_force = 0.0;

  while (is_active ())
    {
      force_ = max (active_blocking_force (), 0.0);
      Real conf = configuration_length ();

      if (conf < line_len_)
	{
	  line_len_force = force_
	    + (line_len_ - conf)
	    * active_springs_stiffness ();
	}

      if (force_ < 1e-8) // ugh.,
	break;

      set_active_states ();
    }

  force_ = line_len_force;
}

/****************************************************************/

Spring_description::Spring_description ()
{
  ideal_ = 0.0;
  inverse_hooke_ = 0.0;
  is_active_ = true;
  block_force_ = 0.0;
}

bool
Spring_description::is_sane () const
{
  return (inverse_hooke_ >= 0)
    && ideal_ > 0
    && !isinf (ideal_) && !isnan (ideal_);
}

Real
Spring_description::length (Real f) const
{
  if (!is_active_)
    f = block_force_;
  return ideal_ + f * inverse_hooke_;
}
/****************************************************************/

/*
  TODO: should a add penalty for widely varying spring forces (caused
  by constraints, eg.


  .     =====
  .     |   |
  .o|o|x ##x
  .

  The ## forces the notes apart; we shouldn't allow the O's to touch
  this closely.
*/
void
Simple_spacer_wrapper::solve (Column_x_positions *positions, bool ragged)
{
  if (ragged)
    spacer_->my_solve_natural_len ();
  else
    spacer_->my_solve_linelen ();

  positions->force_ = spacer_->force_;

  /*
    We used to have a penalty for compression, no matter what, but that
    fucked up wtk1-fugue2 (taking 3 full pages.)
  */
  positions->config_.push (spacer_->indent_);
  for (int i = 0; i < spacer_->springs_.size (); i++)
    {
      Real l = spacer_->springs_[i].length ((ragged) ? 0.0 : spacer_->force_);
      positions->config_.push (positions->config_.top () + l);
      /*
	we have l>= 0 here, up to rounding errors
      */
    }

  /*
    For raggedright, we must have a measure of music density: this is
    to prevent lots of short lines (which all have force = 0).
  */
  if (ragged)
    {
      positions->satisfies_constraints_
	= positions->config_.top () < spacer_->line_len_;
    }
  else
    positions->satisfies_constraints_ = spacer_->is_active ();

  positions->cols_ = spaced_cols_;
  positions->loose_cols_ = loose_cols_;

  /*
    Check if breaking constraints are met.
  */
  bool break_satisfy = true;
  int sz = positions->cols_.size ();
  for (int i = sz; i--;)
    {
      SCM p = positions->cols_[i]->get_property ("penalty");
      if (scm_is_number (p))
	{
	  if (scm_to_double (p) < -9999)
	    break_satisfy = break_satisfy && (i == 0 || i == sz -1);
	  if (scm_to_double (p) > 9999)
	    break_satisfy = break_satisfy && ! (i == 0 || i == sz -1);
	}
    }

  positions->satisfies_constraints_
    = positions->satisfies_constraints_ && break_satisfy;
}

void
Simple_spacer::add_spring (Real ideal, Real inverse_hooke)
{
  Spring_description desc;

  desc.ideal_ = ideal;
  desc.inverse_hooke_ = inverse_hooke;
  if (!desc.is_sane ())
    {
      programming_error ("insane spring found, setting to unit");

      desc.inverse_hooke_ = 1.0;
      desc.ideal_ = 1.0;
    }

  if (!inverse_hooke)
    desc.is_active_ = false;
  else
    {
      /*
	desc.is_active_ ?
      */
      desc.block_force_ = -desc.ideal_ / desc.inverse_hooke_;
      // block at distance 0

      active_count_++;
    }
  springs_.push (desc);
}

static int
compare_paper_column_rank (Grob *const &a,
			   Grob *const &b)
{
  return Paper_column::get_rank (a) - Paper_column::get_rank (b);
}

void
Simple_spacer_wrapper::add_columns (Link_array<Grob> const &icols)
{
  Link_array<Grob> cols (icols);
  cols.clear ();

  for (int i = 0; i < icols.size (); i++)
    if (scm_is_pair (icols[i]->get_object ("between-cols")))
      loose_cols_.push (icols[i]);
    else
      cols.push (icols[i]);

  spaced_cols_ = cols;
  for (int i = 0; i < cols.size () - 1; i++)
    {
      Spring_smob *spring = 0;

      for (SCM s = cols[i]->get_object ("ideal-distances");
	   !spring && scm_is_pair (s);
	   s = scm_cdr (s))
	{
	  Spring_smob *sp = unsmob_spring (scm_car (s));

	  if (sp->other_ == cols[i + 1])
	    spring = sp;
	}

      if (!spring)
	programming_error (_f ("No spring between column %d and next one",
			       Paper_column::get_rank (cols[i])));

      Real ideal = (spring) ? spring->distance_ : spacer_->default_space_;
      Real inverse_hooke = (spring) ? spring->inverse_strength_ : 1.0;

      spacer_->add_spring (ideal, inverse_hooke);
    }

  for (int i = 0; i < cols.size () - 1; i++)
    {
      for (SCM s = Spaceable_grob::get_minimum_distances (cols[i]);
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  Grob *other = unsmob_grob (scm_caar (s));
	  int j = binsearch_links (cols, other, &compare_paper_column_rank);
	  if (j >= 0 && cols[j] == other)
	    spacer_->add_rod (i, j, scm_to_double (scm_cdar (s)));
	}

      if (i
	  && to_boolean (cols[i]->get_property ("keep-inside-line")))
	{
	  Interval e = cols[i]->extent (cols[i], X_AXIS);
	  if (!e.is_empty ())
	    {
	      spacer_->add_rod (i, cols.size () - 1, e[RIGHT]);
	      spacer_->add_rod (0, i, e[LEFT]);
	    }
	}
    }
}

Simple_spacer_wrapper::Simple_spacer_wrapper ()
{
  spacer_ = new Simple_spacer ();
}

Simple_spacer_wrapper::~Simple_spacer_wrapper ()
{
  delete spacer_;
}

Simple_spacer_wrapper::Simple_spacer_wrapper (Simple_spacer_wrapper const &)
{
}
