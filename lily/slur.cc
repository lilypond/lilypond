/*
  slur.cc -- implement  Slur

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
    Jan Nieuwenhuizen <janneke@gnu.org>
*/

/*
  [TODO]
    * begin and end should be treated as a/acknowledge Scripts.
    * broken slur should have uniform trend
 */

#include "slur.hh"
#include "scalar.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "note-column.hh"
#include "stem.hh"
#include "paper-column.hh"
#include "molecule.hh"
#include "debug.hh"
#include "box.hh"
#include "bezier.hh"
#include "encompass-info.hh"
#include "main.hh"


Slur::Slur ()
{
}

void
Slur::add_column (Note_column*n)
{
  if (!n->head_l_arr_.size ())
    warning (_ ("Putting slur over rest. Ignoring"));
  else
    {
      encompass_arr_.push (n);
      add_dependency (n);
    }
}

Direction
Slur::get_default_dir () const
{
  Direction d = DOWN;
  for (int i=0; i < encompass_arr_.size (); i ++) 
    {
      if (encompass_arr_[i]->dir () < 0) 
	{
	  d = UP;
	  break;
	}
    }
  return d;
}

void
Slur::do_add_processing ()
{
  set_bounds (LEFT, encompass_arr_[0]);    
  if (encompass_arr_.size () > 1)
    set_bounds (RIGHT, encompass_arr_.top ());
}

void
Slur::do_pre_processing ()
{
  // don't set directions
}

void
Slur::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  int i;
  while ((i = encompass_arr_.find_i (dynamic_cast<Note_column *> (o))) >=0) 
    {
      if (n)
	encompass_arr_[i] = dynamic_cast<Note_column *> (n);
      else
	encompass_arr_.del (i);
    }
}

static int 
Note_column_compare (Note_column *const&n1 , Note_column* const&n2)
{
  return Item::left_right_compare (n1, n2);
}

void
Slur::do_post_processing ()
{
  encompass_arr_.sort (Note_column_compare);
  if (!dir_)
    dir_ = get_default_dir ();

  /* 
   Slur and tie placement [OSU]

   Slurs:
   * x = centre of head - d * x_gap_f

   TODO:
   * y = length < 5ss : horizontal tangent + d * 0.25 ss
     y = length >= 5ss : y next interline - d * 0.25 ss
   */

  Real interline_f = paper_l ()->get_realvar (interline_scm_sym);
  Real internote_f = interline_f / 2;

  Real x_gap_f = paper_l ()->get_var ("slur_x_gap");
  Real y_gap_f = paper_l ()->get_var ("slur_y_gap");

  Drul_array<Note_column*> note_column_drul;
  note_column_drul[LEFT] = encompass_arr_[0];
  note_column_drul[RIGHT] = encompass_arr_.top ();

  bool fix_broken_b = false;
  Direction d = LEFT;
  do 
    {
      dx_f_drul_[d] = dy_f_drul_[d] = 0;
      if ((note_column_drul[d] == spanned_drul_[d])
	  && note_column_drul[d]->head_l_arr_.size ()
	  && (note_column_drul[d]->stem_l_))
	{
	  Stem* stem_l = note_column_drul[d]->stem_l_;
	  /*
	    side directly attached to note head;
	    no beam getting in the way
	  */
	  if ((stem_l->extent (Y_AXIS).empty_b ()
	       || !((stem_l->dir_ == dir_) && (dir_ != d)))
	      && !((dir_ == stem_l->dir_)
		   && stem_l->beam_l_ && (stem_l->beams_i_drul_[-d] >= 1)))
	    {
	      dx_f_drul_[d] = spanned_drul_[d]->extent (X_AXIS).length () / 2;
	      dx_f_drul_[d] -= d * x_gap_f;

	      if (stem_l->dir_ != dir_)
		{
		  dy_f_drul_[d] = note_column_drul[d]->extent (Y_AXIS)[dir_];
		}
	      else
		{
		  dy_f_drul_[d] = stem_l->chord_start_f ()
		    + dir_ * internote_f;
		}
	      dy_f_drul_[d] += dir_ * y_gap_f;
	    }
	  /*
	    side attached to (visible) stem
	  */
	  else
	    {
	      dx_f_drul_[d] = stem_l->hpos_f ()
		- spanned_drul_[d]->absolute_coordinate (X_AXIS);
	      /*
		side attached to beamed stem
	       */
	      if (stem_l->beam_l_ && (stem_l->beams_i_drul_[-d] >= 1))
		{
		  dy_f_drul_[d] = stem_l->extent (Y_AXIS)[dir_];
		  dy_f_drul_[d] += dir_ * 2 * y_gap_f;
		}
	      /*
		side attached to notehead, with stem getting in the way
	       */
	      else
		{
		  dx_f_drul_[d] -= d * x_gap_f;
		  
		  dy_f_drul_[d] = stem_l->chord_start_f ()
		    + dir_ * internote_f;
		  dy_f_drul_[d] += dir_ * y_gap_f;
		}
	    }
	}
      /*
	loose end
      */
      else
	{
	  /*
	    need break-align too.  what about other spanners?
	   */
	  if (d == LEFT)
	    dx_f_drul_[d] = spanned_drul_[LEFT]->extent (X_AXIS).length ();
	  	
	  /*
	    broken: should get y from other piece, so that slur
	    continues up/down trend

	    for now: be horizontal..
	  */
	  fix_broken_b = true;
	}
    }
  while (flip (&d) != LEFT);

  int interstaff_i = 0;
  for (int i = 0; i < encompass_arr_.size (); i++)
    {
      Encompass_info info (encompass_arr_[i], dir_, this);
      if (info.interstaff_f_)
	{
	  interstaff_i++;
	}
    }
  bool interstaff_b = interstaff_i && (interstaff_i < encompass_arr_.size ());

  Drul_array<Encompass_info> info_drul;
  info_drul[LEFT] = Encompass_info (encompass_arr_[0], dir_, this);
  info_drul[RIGHT] = Encompass_info (encompass_arr_.top (), dir_, this);
  Real interstaff_f = info_drul[RIGHT].interstaff_f_
    - info_drul[LEFT].interstaff_f_;

  if (fix_broken_b)
    {
      Direction d = (encompass_arr_.top () != spanned_drul_[RIGHT]) ?
	RIGHT : LEFT;
      dy_f_drul_[d] = info_drul[d].o_[Y_AXIS];
      if (!interstaff_b)
	{
	  dy_f_drul_[d] -= info_drul[d].interstaff_f_;
	  
	  if (interstaff_i)
	    {
	      dy_f_drul_[LEFT] += info_drul[d].interstaff_f_;
	      dy_f_drul_[RIGHT] += info_drul[d].interstaff_f_;
	    }
	}
    }
	

  /*
    Now we've got a fine slur
    Catch and correct some ugly cases
   */

  Real height_damp_f;
  Real slope_damp_f;
  Real snap_f;
  Real snap_max_dy_f;

  if (!interstaff_b)
    {
      height_damp_f = paper_l ()->get_var ("slur_height_damping");
      slope_damp_f = paper_l ()->get_var ("slur_slope_damping");
      snap_f = paper_l ()->get_var ("slur_snap_to_stem");
      snap_max_dy_f = paper_l ()->get_var ("slur_snap_max_slope_change");
    }
  else
    {
      height_damp_f = paper_l ()->get_var ("slur_interstaff_height_damping");
      slope_damp_f = paper_l ()->get_var ("slur_interstaff_slope_damping");
      snap_f = paper_l ()->get_var ("slur_interstaff_snap_to_stem");
      snap_max_dy_f = paper_l ()->get_var ("slur_interstaff_snap_max_slope_change");
    }

  if (!fix_broken_b)
    dy_f_drul_[RIGHT] += interstaff_f;

  Real dy_f = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
  if (!fix_broken_b)
    dy_f -= interstaff_f;
  Real dx_f = do_width ().length () + dx_f_drul_[RIGHT] - dx_f_drul_[LEFT];

  /*
    Avoid too steep slurs.
   */
  Real slope_ratio_f = abs (dy_f / dx_f);
  if (slope_ratio_f > slope_damp_f)
    {
      Direction d = (Direction)(- dir_ * (sign (dy_f)));
      if (!d)
	d = LEFT;
      Real damp_f = (slope_ratio_f - slope_damp_f) * dx_f;
      /*
	must never change sign of dy
       */
      damp_f = damp_f <? abs (dy_f);
      dy_f_drul_[d] += dir_ * damp_f;
    }

  /*
   Avoid too high slurs 

   Wierd slurs may look a lot better after they have been
   adjusted a bit.
   So, we'll do this in 3 steps
   */
  for (int i = 0; i < 3; i++)
    {
      Drul_array<Interval> curve_xy_drul = curve_extent_drul ();
      Real height_f = curve_xy_drul[Y].length ();
      Real width_f = curve_xy_drul[X].length ();
      
      dy_f = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
      if (!fix_broken_b)
	dy_f -= interstaff_f;

      Real height_ratio_f = abs (height_f / width_f);
      if (height_ratio_f > height_damp_f)
	{
	  Direction d = (Direction)(- dir_ * (sign (dy_f)));
	  if (!d)
	    d = LEFT;
	  /* take third step */
	  Real damp_f = (height_ratio_f - height_damp_f) * width_f / 3;
	  /*
	    if y positions at about the same height, correct both ends
	  */
	  if (abs (dy_f / dx_f ) < slope_damp_f)
	    {
	      dy_f_drul_[-d] += dir_ * damp_f;
	      dy_f_drul_[d] += dir_ * damp_f;
	    }
	  /*
	    don't change slope too much, would have been catched by slope damping
	  */
	  else
	    {
	      damp_f = damp_f <? abs (dy_f/2);
	      dy_f_drul_[d] += dir_ * damp_f;
	    }
	}
    }

  /*
    If, after correcting, we're close to stem-end...
  */
  Drul_array<Real> snapy_f_drul;
  snapy_f_drul[LEFT] = snapy_f_drul[RIGHT] = 0;
  Drul_array<Real> snapx_f_drul;
  snapx_f_drul[LEFT] = snapx_f_drul[RIGHT] = 0;
  Drul_array<bool> snapped_b_drul;
  snapped_b_drul[LEFT] = snapped_b_drul[RIGHT] = false;
  do
    {
      if ((note_column_drul[d] == spanned_drul_[d])
	  && (note_column_drul[d]->stem_l_)
	  && (note_column_drul[d]->stem_l_->dir_ == dir_)
	  && (abs (note_column_drul[d]->stem_l_->extent (Y_AXIS)[dir_]
		   - dy_f_drul_[d] + (d == LEFT ? 0 : interstaff_f))
	      <= snap_f))
	{
	  /*
	    prepare to attach to stem-end
	  */
	  Stem* stem_l = note_column_drul[d]->stem_l_;
	  snapx_f_drul[d] = stem_l->hpos_f ()
	    - spanned_drul_[d]->absolute_coordinate (X_AXIS);
	  snapy_f_drul[d] = stem_l->extent (Y_AXIS)[dir_];
	  snapy_f_drul[d] += info_drul[d].interstaff_f_;
	  snapy_f_drul[d] += dir_ * 2 * y_gap_f;
	  snapped_b_drul[d] = true;
	}
    }
  while (flip (&d) != LEFT);

  /*
    only use snapped positions if sign (dy) will not change
    and dy doesn't change too much
    */
  if (!fix_broken_b)
    dy_f += interstaff_f;
  if (snapped_b_drul[LEFT] && snapped_b_drul[RIGHT]
      && ((sign (snapy_f_drul[RIGHT] - snapy_f_drul[LEFT]) == sign (dy_f)))
      && (!dy_f || (abs (snapy_f_drul[RIGHT] - snapy_f_drul[LEFT] - dy_f)
		    < abs (dy_f * snap_max_dy_f))))
    {
      do
	{
	  dy_f_drul_[d] = snapy_f_drul[d];
	  dx_f_drul_[d] = snapx_f_drul[d];
	}
      while (flip (&d) != LEFT);
  
    }
  else if (snapped_b_drul[LEFT]
      && ((sign (dy_f_drul_[RIGHT] - snapy_f_drul[LEFT]) == sign (dy_f)))
      && (!dy_f || (abs (dy_f_drul_[RIGHT] - snapy_f_drul[LEFT] - dy_f)
		    < abs (dy_f * snap_max_dy_f))))
    {
      Direction d = LEFT;
      dy_f_drul_[d] = snapy_f_drul[d];
      dx_f_drul_[d] = snapx_f_drul[d];
    }
  else if (snapped_b_drul[RIGHT]
      && ((sign (snapy_f_drul[RIGHT] - dy_f_drul_[LEFT]) == sign (dy_f)))
      && (!dy_f || (abs (snapy_f_drul[RIGHT] - dy_f_drul_[LEFT] - dy_f)
		    < abs (dy_f * snap_max_dy_f))))
    {
      Direction d = RIGHT;
      dy_f_drul_[d] = snapy_f_drul[d];
      dx_f_drul_[d] = snapx_f_drul[d];
    }
}

Array<Offset>
Slur::get_encompass_offset_arr () const
{
  Array<Offset> offset_arr;
#if 0
  /*
    check non-disturbed slur
    FIXME: x of ends off by a tiny bit!!
  */
  offset_arr.push (Offset (0, dy_f_drul_[LEFT]));
  offset_arr.push (Offset (0, dy_f_drul_[RIGHT]));
  return offset_arr;
#endif

  int interstaff_i = 0;
  for (int i = 0; i < encompass_arr_.size (); i++)
    {
      Encompass_info info (encompass_arr_[i], dir_, this);
      if (info.interstaff_f_)
	{
	  interstaff_i++;
	}
    }
  bool interstaff_b = interstaff_i && (interstaff_i < encompass_arr_.size ());
  
  Offset origin (absolute_coordinate (X_AXIS), 0);

  int first = 1;
  int last = encompass_arr_.size () - 2;

  offset_arr.push (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));
  /*
    left is broken edge
  */
  if (encompass_arr_[0] != spanned_drul_[LEFT])
    {
      first--;
      Encompass_info left_info (encompass_arr_[0], dir_, this);
      if (interstaff_b)
	offset_arr[0][Y_AXIS] += left_info.interstaff_f_;
    }

  /*
    right is broken edge
  */
  if (encompass_arr_.top () != spanned_drul_[RIGHT])
    {
      last++;
    }

  for (int i = first; i <= last; i++)
    {
      Encompass_info info (encompass_arr_[i], dir_, this);
      offset_arr.push (info.o_ - origin);
    }

  offset_arr.push (Offset (do_width ().length () + dx_f_drul_[RIGHT],
			   dy_f_drul_[RIGHT]));

  return offset_arr;
}


Array<Rod>
Slur::get_rods () const
{
  Array<Rod> a;
  Rod r;
  r.item_l_drul_ = spanned_drul_;
  r.distance_f_ = paper_l ()->get_var ("slur_x_minimum");

  a.push (r);
  return a;
}

