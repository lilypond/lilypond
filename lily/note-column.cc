/*
  note-column.cc -- implement Note_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "dot-column.hh"
#include "note-column.hh"
#include "beam.hh"
#include "note-head.hh"
#include "stem.hh"
#include "rest.hh"
#include "debug.hh"
#include "paper-def.hh"

bool
Note_column::rest_b () const
{
  return rest_l_arr_.size ();
}

int
Note_column::shift_compare (Note_column *const &p1, Note_column*const&p2)
{
  SCM s1 = p1->get_elt_property (horizontal_shift_scm_sym);
  SCM s2 = p2->get_elt_property (horizontal_shift_scm_sym);

  int h1 = (s1 == SCM_BOOL_F) ? 0 : gh_scm2int (SCM_CDR(s1));
  int h2 = (s2 == SCM_BOOL_F) ? 0 : gh_scm2int (SCM_CDR(s2));
  return h1 - h2;
}

Note_column::Note_column()
{
  set_axes (X_AXIS,X_AXIS);
  stem_l_ = 0;
}

void
Note_column::sort()
{
  head_l_arr_.sort (Note_head::compare);
}
  
Slice
Note_column::head_positions_interval() const
{
  Slice  iv;

  iv.set_empty ();
  for (int i=0; i <head_l_arr_.size ();i ++)
    {
      int j = head_l_arr_[i]->position_i_;
      iv.unite (Slice (j,j));
    }
  return iv;
}

Direction
Note_column::dir () const
{
  if (stem_l_)
    return stem_l_->dir_;
  else if (head_l_arr_.size ())
    return sign (head_positions_interval().center ());

  assert (false);
  return CENTER;
}


void
Note_column::set_stem (Stem * stem_l)
{
  stem_l_ = stem_l;
  add_dependency (stem_l);
  add_element (stem_l);
}


void
Note_column::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  if (stem_l_ == o) 
    {
      stem_l_ = n ? dynamic_cast<Stem *> (n):0;
    }
  if (dynamic_cast<Note_head *> (o))
    {
      head_l_arr_.substitute (dynamic_cast<Note_head *> (o), 
			      (n)? dynamic_cast<Note_head *> (n) : 0);
    }

  if (dynamic_cast<Rest *> (o)) 
    {
      rest_l_arr_.substitute (dynamic_cast<Rest *> (o), 
			      (n)? dynamic_cast<Rest *> (n) : 0);
    }
}

void
Note_column::add_head (Rhythmic_head *h)
{
  if (Rest*r=dynamic_cast<Rest *> (h))
    {
      rest_l_arr_.push (r);
    }
  if (Note_head *nh=dynamic_cast<Note_head *> (h))
    {
      head_l_arr_.push (nh);
    }
  add_element (h);
}

/**
  translate the rest symbols
 */
void
Note_column::translate_rests (int dy_i)
{
  invalidate_cache (Y_AXIS);
  for (int i=0; i < rest_l_arr_.size(); i++)
    rest_l_arr_[i]->position_i_ += dy_i;
}

void
Note_column::do_print() const
{
#ifndef NPRINT
  DOUT << "rests: " << rest_l_arr_.size() << ", ";
  DOUT << "heads: " << head_l_arr_.size();
#endif
}

void
Note_column::set_dotcol (Dot_column *d)
{
  add_element (d);
}

  /*
    [TODO]
    handle rest under beam (do_post: beams are calculated now)
    what about combination of collisions and rest under beam.

    Should lookup
    
      rest -> stem -> beam -> interpolate_y_position ()
    
   */

void
Note_column::do_post_processing ()
{
  if (!stem_l_ || !rest_b ())
    return;

  Beam * b = stem_l_->beam_l_;
  if (!b)
    return;
      
      /* ugh. Should be done by beam. */
  Real x = stem_l_->hpos_f ();
  Direction d = stem_l_->get_dir ();
  Real beamy = x * b->slope_f_ + b->left_y_;
  Interval restdim = extent (Y_AXIS);

  Real staff_space = rest_l_arr_[0]->staff_line_leading_f ();      
  Real internote_f = staff_space/2;
  Real minimum_dist
    = paper_l ()->get_var ("restcollision_minimum_beamdist") * internote_f;
  Real dist =
    minimum_dist +  -d  * (beamy - restdim[d]) >? 0;

  int stafflines = rest_l_arr_[0]->lines_i ();
      
  // move discretely by half spaces.
  int discrete_dist = int (ceil (dist / (0.5 *staff_space)));

  // move by whole spaces inside the staff.
  if (discrete_dist < stafflines+1)
    discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);

  translate_rests (-d *  discrete_dist);
}
