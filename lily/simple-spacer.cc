/*   
  simple-spacer.cc -- implement Simple_spacer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>

  TODO:
  - add support for different stretch/shrink constants?
  
*/
#include <stdio.h>
#include <math.h>
#include <libc-extension.hh>	// isinf

#include "simple-spacer.hh"
#include "paper-column.hh"
#include "spring.hh"
#include "rod.hh"
#include "warn.hh"
#include "column-x-positions.hh"
#include "spaceable-grob.hh"
#include "dimensions.hh"


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


Simple_spacer::Simple_spacer ()
{
  /*
    Give an extra penalty for compression. Needed to avoid compressing
    tightly spaced lines.
  */
  active_count_ = 0;
  force_ = 0.;
  indent_ =0.0;
  default_space_ = 20 PT;
}

void
Simple_spacer::add_rod (int l, int r, Real dist)
{
  if (isinf (dist) || isnan (dist))
    {
      programming_error ("Weird minimum distance. Ignoring");
      return;
    }

  Real c = range_stiffness (l,r);
  if (isinf (c))
    {
      /*
	If a spring is fixed, we have to do something here:
	we let the rod override the spring. 
       */
      Real total_dist = 0.;
      for (int i = l ; i < r; i++)
	total_dist += springs_[i].ideal_;

      if (total_dist < dist)
	for (int i = l ; i < r; i++)
	  springs_[i].ideal_ *= dist/total_dist;

      return;
    }
  
  Real d = range_ideal_len (l,r);
  Real block_stretch = dist - d;
  
  Real block_force = c * block_stretch;
  force_ = force_ >? block_force;

  for (int i=l; i < r; i++)
    springs_[i].block_force_ = block_force >?
      springs_[i].block_force_ ;
}

Real
Simple_spacer::range_ideal_len (int l, int r)   const
{
  Real d =0.;
  for (int i=l; i < r; i++)
    d += springs_[i].ideal_;
  return d;
}

Real
Simple_spacer::range_stiffness (int l, int r) const
{
  Real den =0.0;
  for (int i=l; i < r; i++)
    {
      if (springs_[i].is_active_)
	den += 1 / springs_[i].hooke_;
    }

  return 1 / den;
}

Real
Simple_spacer::active_blocking_force () const
{
  Real bf = - infinity_f; 
  for (int i=0; i < springs_.size (); i++)
    if (springs_[i].is_active_)
      {
	bf = bf >? springs_[i].block_force_;
      }
  return bf;
}

Real
Simple_spacer::active_springs_stiffness () const
{
  Real  stiff =  range_stiffness (0, springs_.size ());
  if (isinf (stiff))
    {
      /*
	all springs are inactive. Take the stiffness of the
	latest spring to block.
       */

      Real max_block_force = -infinity_f;
      int max_i = -1;
      for (int i=0; i < springs_.size (); i++)
	{
	  if (springs_[i].block_force_ > max_block_force)
	    {
	      max_i = i;
	      max_block_force = springs_[i].block_force_;
	    }
	}

      stiff = springs_[max_i].hooke_;	 
    }
  return stiff;
}

void
Simple_spacer::set_active_states ()
{
  /* float comparison is safe, since force is only copied.  */
  for (int i=0 ; i <springs_.size (); i++)
    if (springs_[i].is_active_
	&& springs_[i].block_force_ >= force_)
      {
	springs_[i].is_active_ = false;
	active_count_ --; 
      }
}   

Real
Simple_spacer::configuration_length () const
{
  Real l =0.;
  for (int i=0; i < springs_.size (); i++)
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
      force_ = active_blocking_force () >? 0.0;
      Real conf = configuration_length ();

      if (conf < line_len_)
	{
	  line_len_force = force_
	    + (line_len_ - conf)
	    * active_springs_stiffness();
	}
      
      if (force_ < 1e-8) // ugh.,
	break;
      
      set_active_states ();
    }

  force_ = line_len_force;
}

LY_DEFINE(ly_solve_spring_rod_problem, "ly:solve-spring-rod-problem",
	  4, 1, 0, (SCM springs, SCM rods, SCM length, SCM ragged),
	  "Solve a spring and rod problem for @var{count} objects, that "
	  "are connected by @var{count-1} springs, and an arbitrary number of rods "
	  "Springs have the format (ideal, hooke) and rods (idx1, idx2, distance) "
	  "@var{length} is a number, @var{ragged} a boolean "
	  "Return: a list containing the force (#f for non-satisfied constraints) "
	  "followed by the @var{spring-count}+1 positions of the objects. "
	  )
{
  int len = scm_ilength (springs);
  if (len == 0)
    return scm_list_2 (scm_from_double (0.0), scm_from_double (0.0));
  
  SCM_ASSERT_TYPE (len >= 0, springs, SCM_ARG1, __FUNCTION__, "list of springs");
  SCM_ASSERT_TYPE (scm_ilength (rods) >= 0, rods, SCM_ARG2, __FUNCTION__, "list of rods");
  SCM_ASSERT_TYPE (scm_is_number (length) || length == SCM_BOOL_F,
		   length, SCM_ARG3, __FUNCTION__, "number or #f");


  bool is_ragged   = ragged == SCM_BOOL_T; 
  Simple_spacer spacer; 
  for (SCM s = springs; ly_c_pair_p (s); s = ly_cdr (s))
    {
      Real ideal = scm_to_double (ly_caar (s));
      Real hooke = scm_to_double (ly_cadar (s));

      spacer.add_spring (ideal, hooke);
    }

  for (SCM s = rods; ly_c_pair_p (s); s = ly_cdr (s))
    {
      SCM entry = ly_car (s);
      int l = scm_to_int (ly_car (entry));
      int r = scm_to_int (ly_cadr (entry));
      entry = ly_cddr (entry);
      
      Real distance = scm_to_double (ly_car (entry));
      spacer.add_rod (l, r, distance);
    }

  spacer.line_len_ = scm_to_double (length);
      
  if (is_ragged)
    spacer.my_solve_natural_len ();
  else
    spacer.my_solve_linelen ();

  Array<Real> posns;
  posns.push (0.0);
  for (int i = 0; i < spacer.springs_.size(); i++)
    {
      Real l = spacer.springs_[i].length ((is_ragged) ? 0.0 : spacer.force_);
      posns.push (posns.top() + l);
    }

  SCM force_return = SCM_BOOL_F;
  if (!isinf (spacer.force_)
      && spacer.is_active ())
    {
      force_return = scm_from_double (spacer.force_);
    }
  
  SCM retval= SCM_EOL;
  for (int i = posns.size(); i--;)
    {
      retval = scm_cons (scm_from_double (posns[i]), retval); 
    }

  retval = scm_cons (force_return, retval);
  return retval;  
}
	  
	  
/****************************************************************/

Spring_description::Spring_description ()
{
  ideal_ =0.0;
  hooke_ =0.0;
  is_active_ = true;
  block_force_ = 0.0;
}


bool
Spring_description::is_sane () const
{
  return (hooke_ > 0)
    && ideal_ > 0
    && !isinf (ideal_) && !isnan (ideal_);
}

Real
Spring_description::length (Real f) const
{
  if (!is_active_)
    f = block_force_;
  return ideal_ + f / hooke_ ;
}
/****************************************************************/


/*
  
  TODO: should a add penalty for widely varying spring forces (caused
  by constraints, eg.


         =====  
         |   |
  o|o|  x ##x


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
  for (int i=0; i < spacer_->springs_.size (); i++)
    {
      Real  l = spacer_->springs_[i].length ((ragged) ? 0.0 : spacer_->force_);
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
      positions->satisfies_constraints_ = 
	positions->config_.top () < spacer_->line_len_ ;
    }


  positions->cols_ = spaced_cols_;
  positions->loose_cols_ = loose_cols_;
  positions->satisfies_constraints_ =
    positions->satisfies_constraints_ && spacer_->is_active ();

  /*
    Check if breaking constraints are met.
   */
  bool break_satisfy = true;
  int sz =  positions->cols_.size ();
  for (int i = sz; i--; )
    {
      SCM p = positions->cols_[i]->get_property ( "penalty");
      if (scm_is_number (p))
	{
	  if (scm_to_double (p) < -9999)
	    break_satisfy = break_satisfy && (i == 0 || i == sz -1);
	  if (scm_to_double (p) > 9999)
	    break_satisfy = break_satisfy && !(i == 0 || i == sz -1);
	}
      
    }

  positions->satisfies_constraints_ =
    positions->satisfies_constraints_ && break_satisfy;
}

void
Simple_spacer::add_spring (Real ideal, Real hooke)
{
  Spring_description desc;

  desc.ideal_ = ideal;
  desc.hooke_ = hooke;
  if (!desc.is_sane ())
    {
      programming_error ("Insane spring found. Setting to unit spring.");

      desc.hooke_ = 1.0;
      desc.ideal_ = 1.0;
    }
  
  if (isinf (hooke))
    {
      desc.is_active_ = false;
    }
  else
    {
      /*
	desc.is_active_ ? 
      */
      desc.block_force_ = - desc.hooke_ * desc.ideal_; // block at distance 0
      
      active_count_ ++;
    }
  springs_.push (desc);
}

void
Simple_spacer_wrapper::add_columns (Link_array<Grob> const &icols)
{
  Link_array<Grob> cols (icols);
  
  for (int i =  cols.size (); i--;)
    if (ly_c_pair_p (cols[i]->get_property ("between-cols")))
      {
	loose_cols_.push (cols[i]);
	cols.del (i);
      }
  
  spaced_cols_ = cols;
  for (int i=0; i < cols.size () - 1; i++)
    {
      Spring_smob *spring = 0;

      for (SCM s = cols[i]->get_property ("ideal-distances");
	   !spring && ly_c_pair_p (s);
	   s = ly_cdr (s))
	{
	  Spring_smob *sp = unsmob_spring (ly_car (s));
	  
	  
	  if (sp->other_ == cols[i+1])
	    spring = sp;
	}

      if (!spring)
	programming_error (_f ("No spring between column %d and next one",
			       Paper_column::get_rank (cols[i])
			       ));

      Real ideal = (spring) ? spring->distance_ : spacer_->default_space_;
      Real hooke = (spring) ? spring->strength_ : 1.0;
	
      spacer_->add_spring (ideal, hooke);
    }
  
  for (int i=0; i < cols.size () - 1; i++)
    {
      for (SCM s = Spaceable_grob::get_minimum_distances (cols[i]);
	   ly_c_pair_p (s); s = ly_cdr (s))
	{
	  Grob * other = unsmob_grob (ly_caar (s));
	  int oi = cols.find_index (other);
	  if (oi >= 0)
	    {
	      spacer_->add_rod (i, oi, scm_to_double (ly_cdar (s)));
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


Simple_spacer_wrapper::Simple_spacer_wrapper (Simple_spacer_wrapper const&)
{
}
