/*
  tie.cc -- implement Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "lookup.hh"
#include "paper-def.hh"
#include "tie.hh"
#include "note-head.hh"
#include "bezier.hh"
#include "paper-column.hh"
#include "debug.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "molecule.hh"
#include "bezier-bow.hh"
#include "stem.hh"

void
Tie::set_head (Direction d, Item * head_l)
{
  assert (!head (d));
  index_set_cell (get_elt_property ("heads"), d, head_l->self_scm_);
  
  set_bounds (d, head_l);
  add_dependency (head_l);
}

Tie::Tie()
{
  set_elt_property ("heads", gh_cons (SCM_EOL, SCM_EOL));
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;

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
  Stem * sl =  head(LEFT) ? head (LEFT)->stem_l () :0;
  Stem * sr =  head(RIGHT) ? head (RIGHT)->stem_l () :0;  

  if (sl && directional_element (sl).get () == UP
      && sr && directional_element (sr).get () == UP)
    return DOWN;
  else
    return UP;

#if 0 
  Real p1 = Staff_symbol_referencer_interface (head (LEFT)).position_f () ;
  Real p2 = Staff_symbol_referencer_interface (head (RIGHT)).position_f () ;  
  
  int m = int (p1  + p2);

  /*
    If dir is not determined: inverse of stem: down
    (see stem::get_default_dir ())
   */
  Direction neutral_dir = (Direction)(int)paper_l ()->get_var ("stem_default_neutral_direction");
  return (m == 0) ? other_dir (neutral_dir) : (m < 0) ? DOWN : UP;
#endif
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

  index_set_cell (get_elt_property ("heads"), LEFT, new_head_drul[LEFT]->self_scm_ );
  index_set_cell (get_elt_property ("heads"), RIGHT, new_head_drul[LEFT]->self_scm_ );

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

  if (!directional_element (this).get ())
    directional_element (this).set (get_default_dir ());
  
  Real staff_space = paper_l ()->get_var ("interline");
  Real half_staff_space = staff_space / 2;
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
	  // FIXME extremal deprecated
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

  Real y_f = half_staff_space * ypos; 
  int ypos_i = int (ypos);
 
  Real dx_f = extent (X_AXIS).length () + dx_f_drul_[RIGHT] - dx_f_drul_[LEFT];
  Direction dir = directional_element (this).get();
  if (dx_f < paper_l ()->get_var ("tie_staffspace_length"))
    {
      if (abs (ypos_i) % 2)
	y_f += dir * half_staff_space;
      y_f += dir * y_gap_f;
    }
  else
    {
      if (! (abs (ypos_i) % 2))
	y_f += dir * half_staff_space;
      y_f += dir * half_staff_space;
      y_f -= dir * y_gap_f;
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




Molecule*
Tie::do_brew_molecule_p () const
{
  Real thick = paper_l ()->get_var ("tie_thickness");
  Bezier one = get_curve ();

  Molecule a;
  SCM d =  get_elt_property ("dashed");
  if (gh_number_p (d))
    a = lookup_l ()->dashed_slur (one, thick, gh_scm2int (d));
  else
    a = lookup_l ()->slur (one, directional_element (this).get () * thick, thick);
  
  return new Molecule (a); 
}



Bezier
Tie::get_curve () const
{
  Bezier_bow b (get_encompass_offset_arr (), directional_element (this).get ());

  b.ratio_ = paper_l ()->get_var ("slur_ratio");
  b.height_limit_ = paper_l ()->get_var ("slur_height_limit");
  b.rc_factor_ = paper_l ()->get_var ("slur_rc_factor");

  b.calculate ();
  return b.get_curve ();
}

#if 0

/*
  TODO: FIXME.
 */

/*
  Clipping

  This function tries to address two issues:
    * the tangents of the slur should always point inwards 
      in the actual slur, i.e.  *after rotating back*.

    * slurs shouldn't be too high 
      let's try : h <= 1.2 b && h <= 3 staffheight?

  We could calculate the tangent of the bezier curve from
  both ends going inward, and clip the slur at the point
  where the tangent (after rotation) points up (or inward
  with a certain maximum angle).
  
  However, we assume that real clipping is not the best
  answer.  We expect that moving the outer control point up 
  if the slur becomes too high will result in a nicer slur 
  after recalculation.

  Knowing that the tangent is the line through the first
  two control points, we'll clip (move the outer control
  point upwards) too if the tangent points outwards.
 */

bool
Bezier_Tie::calc_clipping ()
{
  Real clip_height = paper_l_->get_var ("slur_clip_height");
  Real clip_ratio = paper_l_->get_var ("slur_clip_ratio");
  Real clip_angle = paper_l_->get_var ("slur_clip_angle");

  Real b = curve_.control_[3][X_AXIS] - curve_.control_[0][X_AXIS];
  Real clip_h = clip_ratio * b <? clip_height;
  Real begin_h = curve_.control_[1][Y_AXIS] - curve_.control_[0][Y_AXIS];
  Real end_h = curve_.control_[2][Y_AXIS] - curve_.control_[3][Y_AXIS];
  Real begin_dy = 0 >? begin_h - clip_h;
  Real end_dy = 0 >? end_h - clip_h;
  
  Real pi = M_PI;
  Real begin_alpha = (curve_.control_[1] - curve_.control_[0]).arg () + dir_ * alpha_;
  Real end_alpha = pi -  (curve_.control_[2] - curve_.control_[3]).arg () - dir_  * alpha_;

  Real max_alpha = clip_angle / 90 * pi / 2;
  if ((begin_dy < 0) && (end_dy < 0)
    && (begin_alpha < max_alpha) && (end_alpha < max_alpha))
    return false;

  transform_back ();

  if ((begin_dy > 0) || (end_dy > 0))
    {
      Real dy = (begin_dy + end_dy) / 4;
      dy *= cos (alpha_);
      encompass_[0][Y_AXIS] += dir_ * dy;
      encompass_.top ()[Y_AXIS] += dir_ * dy;
    }
  else
    {
      //ugh
      Real c = 0.4;
      if (begin_alpha >= max_alpha)
	begin_dy = 0 >? c * begin_alpha / max_alpha * begin_h;
      if (end_alpha >= max_alpha)
	end_dy = 0 >? c * end_alpha / max_alpha * end_h;

      encompass_[0][Y_AXIS] += dir_ * begin_dy;
      encompass_.top ()[Y_AXIS] += dir_ * end_dy;

      Offset delta = encompass_.top () - encompass_[0];
      alpha_ = delta.arg ();
    }

  to_canonic_form ();

  return true;
}
#endif



Array<Offset>
Tie::get_encompass_offset_arr () const
{
  Array<Offset> offset_arr;
  offset_arr.push (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));
  offset_arr.push (Offset (spanner_length () + dx_f_drul_[RIGHT],
			   dy_f_drul_[RIGHT]));
		      
  return offset_arr;
}


