/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "paper-def.hh"
#include "tie.hh"
#include "note-head.hh"
#include "paper-column.hh"
#include "debug.hh"



void
Tie::set_head (Direction d, Note_head * head_l)
{
  assert (!head_l_drul_[d]);
  head_l_drul_[d] = head_l;
  set_bounds (d, head_l);

  add_dependency (head_l);
}

Tie::Tie()
{
  head_l_drul_[RIGHT] =0;
  head_l_drul_[LEFT] =0;
}


/*
  ugh: direction of the Tie is more complicated.  See [Ross] p136 and further
 */
Direction
Tie::get_default_dir () const
{
  int m = (head_l_drul_[LEFT]->position_i_ 
	  + head_l_drul_[RIGHT]->position_i_) /2;
  /*
    If dir is not determined: inverse of stem: down
    (see stem::get_default_dir ())
   */
  return (m <= 0)? DOWN : UP;
}

void
Tie::do_add_processing()
{
  if (!(head_l_drul_[LEFT] && head_l_drul_[RIGHT]))
    warning (_ ("lonely tie"));

  Direction d = LEFT;
  Drul_array<Note_head *> new_head_drul = head_l_drul_;
  do {
    if (!head_l_drul_[d])
      new_head_drul[d] = head_l_drul_[(Direction)-d];
  } while (flip(&d) != LEFT);
  head_l_drul_ = new_head_drul;
}

void
Tie::do_post_processing()
{
  assert (head_l_drul_[LEFT] || head_l_drul_[RIGHT]);

  Real interline_f = paper_l ()->get_realvar (interline_scm_sym);
  Real internote_f = interline_f / 2;
  Real x_gap_f = paper_l ()->get_var ("tie_x_gap");
  Real y_gap_f = paper_l ()->get_var ("tie_y_gap");

  /* 
   Slur and tie placement [OSU]

   Ties:

       * x = inner vertical tangent - d * gap

   */


  /*
    OSU: not different for outer notes, so why all this code?
    ie,  can we drop this, or should it be made switchable.
   */
#if 0
  Direction d = LEFT;
  do
    {
      Real head_width_f = head_l_drul_[d]
	? head_l_drul_[d]->extent (X_AXIS).length ()
	: 0;
      /*
	side attached to outer (upper or lower) notehead of chord
      */
      if (head_l_drul_[d]
	  /*
	    && head_l_drul_[d]->remove_elt_property (extremal_scm_sym) != SCM_BOOL_F)
	    ugh, ugh:

	        a~a~a;

	    to second tie, middle notehead seems not extremal

	    Getting scared a bit by score-element's comment:
	    // is this a good idea?
	  */
	  && (head_l_drul_[d]->get_elt_property (extremal_scm_sym)
	      != SCM_BOOL_F))
	{
	if (d == LEFT)
	    dx_f_drul_[d] += head_width_f;
	  dx_f_drul_[d] += -d * x_gap_f;
	}
      /*
	side attached to inner notehead
      */
      else
	{
	  dx_f_drul_[d] += -d * head_width_f;
	}
    } while (flip (&d) != LEFT);

#else

  if (head_l_drul_[LEFT])
    dx_f_drul_[LEFT] = head_l_drul_[LEFT]->extent (X_AXIS).length ();
  dx_f_drul_[LEFT] += x_gap_f;
  dx_f_drul_[RIGHT] -= x_gap_f;

#endif

  /* 
   Slur and tie placement [OSU]  -- check this

   Ties:

       * y = dx <  5ss: horizontal tangent
	 y = dx >= 5ss: y next interline - d * 0.25 ss

	 which probably means that OSU assumes that

	    dy <= 5 dx

	 for smal slurs
   */

  int ypos_i = head_l_drul_[LEFT] ? head_l_drul_[LEFT]->position_i_
    : head_l_drul_[RIGHT]->position_i_;

  Real y_f = internote_f * ypos_i; 

  Real dx_f = extent (X_AXIS).length () + dx_f_drul_[RIGHT] - dx_f_drul_[LEFT];
  if (dx_f < paper_l ()->get_var ("tie_staffspace_length"))
    {
      if (abs (ypos_i) % 2)
	y_f += dir_ * internote_f;
      y_f += dir_ * y_gap_f;
    }
  else
    {
      if (! (abs (ypos_i) % 2))
	y_f += dir_ * internote_f;
      y_f += dir_ * internote_f;
      y_f -= dir_ * y_gap_f;
    }
  
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = y_f;
}

void
Tie::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  Note_head *new_l = n ? dynamic_cast<Note_head *> (n) : 0;
  if (dynamic_cast <Item *> (o) == head_l_drul_[LEFT])
    head_l_drul_[LEFT] = new_l;
  else if (dynamic_cast <Item *> (o) == head_l_drul_[RIGHT])
    head_l_drul_[RIGHT] = new_l;
}


Array<Rod>
Tie::get_rods () const
{
  Array<Rod> a;
  Rod r;
  r.item_l_drul_ = spanned_drul_;
  r.distance_f_ = paper_l ()->get_var ("tie_x_minimum");
  a.push (r);
  return a;
}
