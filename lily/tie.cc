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
#include "staff-symbol-referencer.hh"

void
Tie::set_head (Direction d, Note_head * head_l)
{
  assert (!head (d));
  if (d == LEFT)
    gh_set_car_x (get_elt_property ("heads"), head_l->self_scm_ );
  else if (d == RIGHT)
    gh_set_cdr_x (get_elt_property ("heads"), head_l->self_scm_ );
  
  set_bounds (d, head_l);

  add_dependency (head_l);
}

Tie::Tie()
{
  set_elt_property ("heads", gh_cons (SCM_EOL, SCM_EOL));
}

Note_head* 
Tie::head (Direction d) const
{
  SCM c = get_elt_property ("heads");
  c = index_cell (c, d);

  return dynamic_cast<Note_head*> (unsmob_element (c));  
}


/*
  ugh: direction of the Tie is more complicated.  See [Ross] p136 and further
 */
Direction
Tie::get_default_dir () const
{
  Real p1 = Staff_symbol_referencer_interface (head (LEFT)).position_f () ;
  Real p2 = Staff_symbol_referencer_interface (head (RIGHT)).position_f () ;  
  
  int m = int (p1  + p2);

  /*
    If dir is not determined: inverse of stem: down
    (see stem::get_default_dir ())
   */
  Direction neutral_dir = (Direction)(int)paper_l ()->get_var ("stem_default_neutral_direction");
  return (m == 0) ? other_dir (neutral_dir) : (m < 0) ? DOWN : UP;
}

void
Tie::do_add_processing()
{
  if (!(head (LEFT) && head (RIGHT)))
    warning (_ ("lonely tie"));

  Direction d = LEFT;
  Drul_array<Note_head *> new_head_drul;
  new_head_drul[LEFT] = head(LEFT);
  new_head_drul[RIGHT] = head(RIGHT);  
  do {
    if (!head (d))
      new_head_drul[d] = head((Direction)-d);
  } while (flip(&d) != LEFT);

  gh_set_car_x (get_elt_property ("heads"), new_head_drul[LEFT]->self_scm_ );
  gh_set_cdr_x (get_elt_property ("heads"), new_head_drul[RIGHT]->self_scm_ );
}

void
Tie::do_post_processing()
{
  if (!head (LEFT) && !head (RIGHT))
    {
      programming_error ("Tie without heads.");
      set_elt_property ("transparent", SCM_BOOL_T);
      set_empty (X_AXIS);
      set_empty (Y_AXIS);
      return;
    }

  Real interline_f = paper_l ()->get_var ("interline");
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
      Real head_width_f = head (d)
	? head (d)->extent (X_AXIS).length ()
	: 0;
      /*
	side attached to outer (upper or lower) notehead of chord
      */
      if (head (d)
	  /*

	        a~a~a;

	    to second tie, middle notehead seems not extremal

	    Getting scared a bit by score-element's comment:
	    // is this a good idea?
	  */
	  && (head (d)->get_elt_property ("extremal")
	      != SCM_UNDEFINED))
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

  if (head (LEFT))
    dx_f_drul_[LEFT] = head (LEFT)->extent (X_AXIS).length ();
  else
    dx_f_drul_[LEFT] = get_broken_left_end_align ();
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


  Real ypos = head (LEFT)
    ? Staff_symbol_referencer_interface (head (LEFT)).position_f ()
    : Staff_symbol_referencer_interface (head (RIGHT)).position_f () ;  

  Real y_f = internote_f * ypos; 
  int ypos_i = int (ypos);
 
  Real dx_f = extent (X_AXIS).length () + dx_f_drul_[RIGHT] - dx_f_drul_[LEFT];
  if (dx_f < paper_l ()->get_var ("tie_staffspace_length"))
    {
      if (abs (ypos_i) % 2)
	y_f += get_direction () * internote_f;
      y_f += get_direction () * y_gap_f;
    }
  else
    {
      if (! (abs (ypos_i) % 2))
	y_f += get_direction () * internote_f;
      y_f += get_direction () * internote_f;
      y_f -= get_direction () * y_gap_f;
    }
  
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = y_f;
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

