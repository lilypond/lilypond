/*
  bow.cc -- implement Bow

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
      Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "dimension-cache.hh"
#include "bow.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "lookup.hh"
#include "bezier-bow.hh"
#include "main.hh"

Bow::Bow ()
{
  dy_f_drul_[LEFT] = dy_f_drul_[RIGHT] = 0.0;
  dx_f_drul_[LEFT] = dx_f_drul_[RIGHT] = 0.0;
}

Molecule*
Bow::do_brew_molecule_p () const
{
  Real thick = paper_l ()->get_var ("slur_thickness");
  Bezier one = get_curve ();

  Molecule a;
  SCM d =  get_elt_property ("dashed");
  if (gh_number_p (d))
    a = lookup_l ()->dashed_slur (one, thick, gh_scm2int (d));
  else
    a = lookup_l ()->slur (one, get_direction () * thick, thick);
  
  return new Molecule (a); 
}

Offset
Bow::center () const
{
  Real dy = dy_f_drul_[RIGHT] - dy_f_drul_[LEFT];
  Real dx =  extent(X_AXIS).length ();

  return Offset (dx / 2, dy);
}


Interval
Bow::curve_extent (Axis a) const
{
  return get_curve ().extent (a);
}

Bezier
Bow::get_curve () const
{
  Bezier_bow b (paper_l (),
		get_encompass_offset_arr (), get_direction ());
  
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
Bezier_bow::calc_clipping ()
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
Bow::get_encompass_offset_arr () const
{
  Array<Offset> offset_arr;
  offset_arr.push (Offset (dx_f_drul_[LEFT], dy_f_drul_[LEFT]));
  offset_arr.push (Offset (spanner_length () + dx_f_drul_[RIGHT],
			   dy_f_drul_[RIGHT]));
		      
  return offset_arr;
}


