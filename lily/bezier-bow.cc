/*
  bezier.cc -- implement Bezier and Bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>
#include "bezier-bow.hh"
#include "misc.hh"
#include "bezier.hh"
#include "dimensions.hh"
#include "direction.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "main.hh"

void
flipy (Array<Offset>  &c)
{
  for (int i = c.size (); i--;)
    c[i][Y_AXIS] = - c[i][Y_AXIS];
}

void
rotate (Array<Offset> &c, Real phi)
{
  Offset rot (complex_exp (Offset (0, phi)));
  for (int i = 0; i < c.size (); i++)
    c[i] = complex_multiply (rot, c[i]);
}

void
translate (Array<Offset> &c, Offset o)
{
  for (int i = 0; i < c.size (); i++)
    c[i] += o;
}


Bezier_bow::Bezier_bow (Paper_def* paper_l)
{
  paper_l_ = paper_l;
}

void
Bezier_bow::blow_fit ()
{
  Real x1 = encompass_[0][X_AXIS];
  Real x2 = encompass_.top ()[X_AXIS];

  Real factor = 1.0;
  for (int i=1; i < encompass_.size ()-1; i++)
    {
      if (encompass_[i][X_AXIS] > x1 && encompass_[i][X_AXIS] < x2)
	{
	 Real y = curve_.get_other_coordinate (X_AXIS, encompass_[i][X_AXIS]);
	 if (y>0)
	   {
	     Real f = encompass_[i][Y_AXIS] / y;
	     factor = factor >? f;
	   }
	}
    }

  curve_.control_[1][Y_AXIS] *= factor;
  curve_.control_[2][Y_AXIS] *= factor;  
  return_.control_[1][Y_AXIS] *= factor;
  return_.control_[2][Y_AXIS] *= factor;

  curve_.check_sanity ();
}

Real
Bezier_bow::calc_f (Real height)
{
  transform ();
  calc_default (height);

  Real dy = check_fit_f ();
  calc_return (0, 0);

  transform_back ();
  return dy;
}

void
Bezier_bow::calc ()
{
  transform ();
  calc_controls ();
  transform_back ();
}



/*
  [TODO]
    * see if it works
    * document in Documentation/fonts.tex
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

  bool again = true;

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

  transform ();

  return again;
}

void
Bezier_bow::calc_controls ()
{
  for (int i = 0; i < 3; i++)
    {
      
      if (i && !calc_clipping ())
	return;

      /*
	why do we always recalc from 0?
	shouldn't calc_f () be used (too), rather than blow_fit () (only)?
       */
      calc_default (0);
      curve_.check_sanity ();
      if (check_fit_f () > 0)
        {
	  calc_tangent_controls ();
	  blow_fit ();
	}
      else
	{
	  calc_return (0, 0);
	  return;
	}
    }
}

void
Bezier_bow::calc_return (Real begin_alpha, Real end_alpha)
{
  Real thick = paper_l_->get_var ("slur_thickness");

  return_.control_[0] = curve_.control_[3];
  return_.control_[3] = curve_.control_[0];

  return_.control_[1] = curve_.control_[2] - thick * complex_exp (Offset (0, 90 + end_alpha));
  return_.control_[2] = curve_.control_[1] - thick * complex_exp (Offset (0, 90 - begin_alpha));  
}

/*
This function calculates 2 center control points, based on 
  
 See Documentation/fonts.tex
 */
void
Bezier_bow::calc_tangent_controls ()
{
  Real b = curve_.control_[3][X_AXIS];
  Real h = curve_.control_[1][Y_AXIS];
  

  Drul_array<Offset> disturb;
  Drul_array<Real> maxtan;  
  maxtan[LEFT]  = maxtan[RIGHT] = h/(b/2);
  disturb[LEFT]  = disturb[RIGHT] =   Offset (b / 2, h);

  for (int i = 1; i < encompass_.size () -1; i++)
    {
      Real y= encompass_[i][Y_AXIS];
      if (y> 0)
        {
	  Real x = encompass_[i][X_AXIS];
	  
	  Direction d = LEFT;
	  do
	    {
	      // 1 if d == LEFT
	      int k = (1 - d)/2;
	      Real tan = y /  ((1-k)* b - d * x);

	      if (tan > maxtan[d])
		{
		  maxtan[d] = tan;
		  disturb[d] = Offset (x,y);
		}
	    }
	  while (flip (&d)!=LEFT);
	}
    }

  for (int i = 0; i < encompass_.size (); i++ )
    h = h >? encompass_[i][Y_AXIS];

  /*
    The curve will always be under line between curve_.control_0 -> curve_.control_1, so
    make it extra steep by slur_rc_factor
  */
  Real rc_correct = paper_l_->get_var ("slur_rc_factor");
  
  Drul_array<Real> angles;
  Direction d = LEFT;
  do
    {
      maxtan[d] *= rc_correct;
      angles[d] = atan (-d * maxtan[d]);
    }
  while (flip(&d) != LEFT);

  Real rc3 = 0.0;

  // if we have two disturbing points, have line through those...
  if (disturb[LEFT][Y_AXIS] != disturb[RIGHT][Y_AXIS])
    rc3 = (disturb[RIGHT][Y_AXIS] - disturb[LEFT][Y_AXIS]) / (disturb[RIGHT][X_AXIS] - disturb[LEFT][X_AXIS]);

  else
    rc3 = tan ((angles[RIGHT] - angles[LEFT]) / 2);


  // ugh: be less steep
  rc3 /= 2*rc_correct;
  

  Real c2 = -maxtan[RIGHT] * curve_.control_[3][X_AXIS];

  // use highest because rc3 is damped.
  Real maxy = disturb[LEFT][Y_AXIS] >? disturb[RIGHT][Y_AXIS];
  Real c3 = disturb[LEFT][Y_AXIS] > disturb[RIGHT][Y_AXIS] ?
    maxy - rc3 * disturb[LEFT][X_AXIS] :
    maxy - rc3 * disturb[RIGHT][X_AXIS];

  curve_.control_[1][X_AXIS] = c3 / (maxtan[LEFT] - rc3);
  curve_.control_[1][Y_AXIS] = maxtan[LEFT] * curve_.control_[1][X_AXIS];
  
  curve_.control_[2][X_AXIS] = (c3 - c2) / (maxtan[RIGHT] - rc3);
  curve_.control_[2][Y_AXIS] = maxtan[RIGHT] * curve_.control_[2][X_AXIS] + c2;


  curve_.check_sanity();
  
  calc_return (angles[LEFT], angles[RIGHT]);
}

/*
  The maximum amount that the encompass points stick out above the bezier curve.
 */
Real
Bezier_bow::check_fit_f () const
{
  Real dy = 0;
  Real x1 = encompass_[0][X_AXIS];
  Real x2 = encompass_.top ()[X_AXIS];
  for (int i = 1; i < encompass_.size () - 1; i++)
    {
      Real x = encompass_[i][X_AXIS];
      if (x1< x&& x < x2)
	dy = dy >? (encompass_[i][Y_AXIS] - curve_.get_other_coordinate (X_AXIS, x));
    }
  return dy;
}


void
Bezier_bow::set (Array<Offset> points, Direction dir)
{
  dir_ = dir;
  encompass_ = points;
}

void
Bezier_bow::transform ()
{
  origin_ = encompass_[0];
  translate (encompass_,-origin_);

  Offset delta = encompass_.top () - encompass_[0];
  alpha_ = delta.arg ();

  rotate (encompass_, -alpha_);

  if (dir_ == DOWN)
    flipy (encompass_);
}

void
Bezier_bow::transform_back ()
{
  if (dir_ == DOWN)
    {
      curve_.flip (Y_AXIS);
      return_.flip (Y_AXIS);
      flipy (encompass_);
    }

  curve_.rotate (alpha_);
  curve_.translate (origin_);
 
  return_.rotate (alpha_);
  return_.translate (origin_);
 
  rotate (encompass_,alpha_);
  translate (encompass_,origin_);
}

/*
 See Documentation/fonts.tex
 */
void
Bezier_bow::calc_default (Real h)
{
  Real pi = M_PI;

  Real height_limit = paper_l_->get_var ("slur_height_limit");
  Real ratio = paper_l_->get_var ("slur_ratio");

  Real alpha = height_limit * 2.0 / pi;
  Real beta = pi * ratio / (2.0 * height_limit);

  Offset delta (encompass_.top ()[X_AXIS] 
    - encompass_[0][X_AXIS], 0);

  Real b = delta.length ();
  Real indent = alpha * atan (beta * b);
  Real height = indent + h;
 
  curve_.control_ [0] = Offset (0, 0);
  curve_.control_ [1] = Offset (indent, height);
  curve_.control_ [2] = Offset (b - indent, height);
  curve_.control_ [3] = Offset (b, 0);
}


