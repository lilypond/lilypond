/*
  bezier.cc -- implement Bezier and Bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>
#include "bezier.hh"
#include "misc.hh"

#ifndef STANDALONE
#include "dimensions.hh"
#include "direction.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "main.hh"
#define BEZIER_BOW_DOUT if (check_debug && !monitor->silent_b ("Bezier_bow")) cout
#else
#define BEZIER_BOW_DOUT cerr
#endif

void
Curve::flipy ()
{
  for (int i = 0; i < size (); i++)
    (*this)[i].mirror (Y_AXIS);
}

int
Curve::largest_disturbing ()
{
  Real alpha = 0;
  int j = 0;
  for (int i = 1; i < size (); i++)
    {
      if ((*this)[i].y () > 0)
        {
	  Real phi = (*this)[i].y () / (*this)[i].x ();
	  if (phi > alpha)
	    {
	      alpha = phi;
	      j = i;
	    }
	}
    }
  return j;
}

void
Curve::rotate (Real phi)
{
  Offset rot (complex_exp (Offset (0, phi)));
  for (int i = 0; i < size (); i++)
    (*this)[i] = complex_multiply (rot, (*this)[i]);
}

void
Curve::translate (Offset o)
{
  for (int i = 0; i < size (); i++)
    (*this)[i] += o;
}

Bezier::Bezier ()
{
  control_.set_size (4);
}

void
Bezier::calc (int steps)
{       
  steps = steps >? 10;
  curve_.set_size (steps);
  Real dt = 1.0 / curve_.size ();
  Offset c = 3.0 * (control_[1] - control_[0]);
  Offset b = 3.0 * (control_[2] - control_[1]) - c;
  Offset a = control_[3] - (control_[0] + c + b);
  Real t = 0.0;
  for (int i = 0; i < curve_.size (); i++ )
    {    
      curve_[i] = ((a * t + b) * t + c) * t + control_[0];
      t += dt;
    }
}

void
Bezier::set (Array<Offset> points)
{       
  assert (points.size () == 4);
  control_ = points;
}

Real
Bezier::y (Real x)
{
  // ugh
  // bounds func should be templatised to take array of offsets too?
  Array<Real> positions;
  for (int i = 0; i < curve_.size (); i++)
    positions.push (curve_[i].x ());

  Slice slice = get_bounds_slice (positions, x);
  // ugh
  Offset z1 = curve_[0 >? slice.max () - 1];
  Offset z2 = curve_[1 >? slice.max ()];
  Real multiplier = (x - z2.x ()) / (z1.x () - z2.x ());
  Real y = z1.y () * multiplier + (1.0 - multiplier) * z2.y();

  return y;
}


Bezier_bow::Bezier_bow (Paper_def* paper_l)
{
  paper_l_ = paper_l;
  return_.set_size (4);
}

void
Bezier_bow::blow_fit ()
{
  Real dy1 = check_fit_f ();
  if (!dy1)
    return;

  // be careful not to take too big step
  Real f = 0.3;
  Real h1 = dy1 * f;
  control_[1].y () += h1; 
  control_[2].y () += h1; 
  return_[1].y () += h1; 
  return_[2].y () += h1; 

  calc_bezier ();
  Real dy2 = check_fit_f ();
  if (!dy2)
    return;

#ifndef STANDALONE
  Real internote_f = paper_l_->internote_f ();
#else
  Real internote_f = STAFFHEIGHT / 8;
#endif

  Real epsilon = internote_f / 4;
  if (abs (dy2 - dy1) < epsilon)
    return;
  
  /*
    Assume 
      dy = B (h) 
    with 
      B (h) = a * h + b;

    Then we get for h : B (h) = 0

     B(0)  = dy1 = a * 0 + b   =>   b = dy1
     B(h1) = dy2 = a * h1 + b  =>   a * f * dy1 + b = dy2

	 =>

     a * dy1 / 2 + dy1 = dy2  =>  a = (dy2 - dy1) / (f * dy1)
   */

  Real a = (dy2 - dy1) / (f * dy1);
  Real b = dy1;
  Real h = -b / a;

  if (sign (h) != sign (h1))
    return;

  if (sign (h) != sign (h1))
    return;

  control_[1].y () += -h1 +h; 
  control_[2].y () += -h1 +h; 
  return_[1].y () += -h1 +h;
  return_[2].y () += -h1 +h; 
}

void
Bezier_bow::calc_bezier ()
{
  Real s = sqrt (control_[3].x () * control_[3].x () 
    + control_[1].y () * control_[2].y ());
#ifndef STANDALONE
  Real internote = paper_l_->internote_f ();
#else
  Real internote = STAFFHEIGHT / 8;
#endif
  int steps = (int)rint (s / internote);
  Bezier::calc (steps);
}

Real
Bezier_bow::calc_f (Real height)
{
  transform ();
  calc_default (height);

  calc_bezier ();

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
#ifndef STANDALONE
  Real clip_height = paper_l_->get_var ("slur_clip_height");
  Real clip_ratio = paper_l_->get_var ("slur_clip_ratio");
  Real clip_angle = paper_l_->get_var ("slur_clip_angle");
#else
  Real staffsize_f = STAFFHEIGHT;
  Real clip_height = 3.0 * staffsize_f;
  Real clip_ratio = 1.2;
  Real clip_angle = 100;
#endif

  Real b = control_[3].x () - control_[0].x ();
  Real clip_h = clip_ratio * b <? clip_height;
  Real begin_h = control_[1].y () - control_[0].y ();
  Real end_h = control_[2].y () - control_[3].y ();
  Real begin_dy = 0 >? begin_h - clip_h;
  Real end_dy = 0 >? end_h - clip_h;
  
  Real pi = M_PI;
  Real begin_alpha = (control_[1] - control_[0]).arg () + dir_ * alpha_;
  Real end_alpha = pi -  (control_[2] - control_[3]).arg () - dir_ * alpha_;

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
      encompass_[0].y () += dir_ * dy;
      encompass_[encompass_.size () - 1].y () += dir_ * dy;
    }
  else
    {
      //ugh
      Real c = 0.4;
      if (begin_alpha >= max_alpha)
	begin_dy = 0 >? c * begin_alpha / max_alpha * begin_h;
      if (end_alpha >= max_alpha)
	end_dy = 0 >? c * end_alpha / max_alpha * end_h;

      encompass_[0].y () += dir_ * begin_dy;
      encompass_[encompass_.size () - 1].y () += dir_ * end_dy;

      Offset delta = encompass_[encompass_.size () - 1] - encompass_[0];
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

      calc_default (0);
      calc_bezier ();
      
      if (check_fit_bo ())
        {
	  calc_return (0, 0);
	  return;
	}
      calc_tangent_controls ();

      blow_fit ();
      // ugh
      blow_fit ();
    }
}

void
Bezier_bow::calc_return (Real begin_alpha, Real end_alpha)
{
#ifndef STANDALONE
  Real thick = paper_l_->get_var ("slur_thickness");
#else
  Real thick = 1.8 * 0.4 PT;
#endif

  return_[0] = control_[3];
  return_[3] = control_[0];

  return_[1] = control_[2] - thick * complex_exp (Offset (0, 90 + end_alpha));
  return_[2] = control_[1] 
    - thick * complex_exp (Offset (0, 90 - begin_alpha));  
}

/*
 See Documentation/fonts.tex
 */
void
Bezier_bow::calc_tangent_controls ()
{
  Offset ijk_p (control_[3].x () / 2, control_[1].y ());
  BEZIER_BOW_DOUT << "ijk: " << ijk_p.x () << ", " << ijk_p.y () << endl;

  Real default_rc = ijk_p.y () / ijk_p.x ();

  int begin_disturb = encompass_.largest_disturbing ();
  Offset begin_p = begin_disturb ? Offset (encompass_[begin_disturb].x (), 
    encompass_[begin_disturb].y ()) : ijk_p;
  Real begin_rc = begin_p.y () / begin_p.x ();
  if (default_rc > begin_rc)
    {
      begin_p = ijk_p;
      begin_rc = default_rc;
    }

  Curve reversed;
  reversed.set_size (encompass_.size ());
  Real b = control_[3].x ();
  for (int i = 0; i < encompass_.size (); i++ )
    {
      //       b     1  0
      // r  =     -        *  c 
      //       0     0 -1   
      reversed[i].x () = b - encompass_[encompass_.size () - i - 1].x ();
      reversed[i].y () = encompass_[encompass_.size () - i - 1].y ();
    }

  int end_disturb = reversed.largest_disturbing ();
  end_disturb = end_disturb ? encompass_.size () - end_disturb - 1 : 0;
  Offset end_p = end_disturb ? Offset (encompass_[end_disturb].x (), 
    encompass_[end_disturb].y ()) : ijk_p;
  Real end_rc = end_p.y () / (control_[3].x () - end_p.x ());
  if (default_rc > end_rc)
    {
      end_p = ijk_p;
      end_rc = default_rc;
    }
  BEZIER_BOW_DOUT << "begin " << begin_p.x () << ", " << begin_p.y () << endl;
  BEZIER_BOW_DOUT << "end " << end_p.x () << ", " << end_p.y () << endl;

  Real height =control_[1].y (); 
  for (int i = 0; i < encompass_.size (); i++ )
    height = height >? encompass_[i].y ();

  // emperic computer science:
  //   * tangents somewhat steeper than minimal line
#ifndef STANDALONE
  Real internote = paper_l_->internote_f ();
  Real rc_correct = paper_l_->get_var ("slur_rc_factor");
#else
  Real internote = STAFFHEIGHT / 8;
  Real rc_correct = 2.4;
#endif

  begin_rc *= rc_correct;
  end_rc *= rc_correct;
  Real rc1 = begin_rc;
  Real rc2 = -end_rc;
  
  Real begin_alpha = atan (begin_rc);
  Real end_alpha = atan (-end_rc);
  Real theta = (begin_alpha - end_alpha) / 2;

  Real epsilon = internote / 5;

  // if we have two disturbing points, have height line through those...
  if (!((abs (begin_p.x () - end_p.x ()) < epsilon)
    && (abs (begin_p.y () - end_p.y ()) < epsilon)))
      theta = atan (end_p.y () - begin_p.y ()) / (end_p.x () - begin_p.x ());

  Real rc3 = tan (theta);
  // ugh: be less steep
  rc3 /= 2*rc_correct;

  Real c2 = -rc2 * control_[3].x ();
  Real c3 = begin_p.y () > end_p.y () ? begin_p.y () 
    - rc3 * begin_p.x () : end_p.y () - rc3 * end_p.x ();

  BEZIER_BOW_DOUT << "y1 = " << rc1 << " x + 0" << endl;
  BEZIER_BOW_DOUT << "y2 = " << rc2 << " x + " << c2 << endl;
  BEZIER_BOW_DOUT << "y3 = " << rc3 << " x + " << c3 << endl;
  control_[1].x () = c3 / (rc1 - rc3);
  control_[1].y () = rc1 * control_[1].x ();
  control_[2].x () = (c3 - c2) / (rc2 - rc3);
  BEZIER_BOW_DOUT << "c2.x () = " << control_[2].x () << endl;
  BEZIER_BOW_DOUT << "(c3 - c2) = " << (c3 - c2) << endl;
  BEZIER_BOW_DOUT << "(rc2 - rc3) = " << (rc2 - rc3) << endl;
  control_[2].y () = rc2 * control_[2].x () + c2;
  BEZIER_BOW_DOUT << "c2.y ()" << control_[2].y () << endl;

  calc_return (begin_alpha, end_alpha);
}

bool
Bezier_bow::check_fit_bo ()
{
  for (int i = 1; i < encompass_.size () - 1; i++)
    if ((encompass_[i].x () > encompass_[0].x ())
      && (encompass_[i].x () < encompass_[encompass_.size () -1].x ()))
      if (encompass_[i].y () > y (encompass_[i].x ()))
	return false;
  return true;
}

Real
Bezier_bow::check_fit_f ()
{
  Real dy = 0;
  for (int i = 1; i < encompass_.size () - 1; i++)
    if ((encompass_[i].x () > encompass_[0].x ())
      && (encompass_[i].x () < encompass_[encompass_.size () -1].x ()))
      dy = dy >? (encompass_[i].y () - y (encompass_[i].x ()));
  return dy;
}

void
Bezier_bow::set (Array<Offset> points, int dir)
{
  dir_ = dir;
  encompass_ = points;
}

void
Bezier_bow::transform ()
{
  origin_ = encompass_[0];
  encompass_.translate (-origin_);

  Offset delta = encompass_[encompass_.size () - 1] - encompass_[0];
  alpha_ = delta.arg ();

  encompass_.rotate (-alpha_);

  if (dir_ == DOWN)
    encompass_.flipy ();
}

void
Bezier_bow::transform_back ()
{
  if (dir_ == DOWN)
    {
      control_.flipy ();
      return_.flipy ();
      encompass_.flipy ();
    }

  control_.rotate (alpha_);
  control_.translate (origin_);

  return_.rotate (alpha_);
  return_.translate (origin_);

  encompass_.rotate (alpha_);
  encompass_.translate (origin_);
}

/*
 See Documentation/fonts.tex
 */
void
Bezier_bow::calc_default (Real h)
{
  Real pi = M_PI;
#ifndef STANDALONE
  Real height_limit = paper_l_->get_var ("slur_height_limit");
  Real ratio = paper_l_->get_var ("slur_ratio");
#else
  Real staffsize_f = STAFFHEIGHT;
  Real height_limit = staffsize_f;
  Real ratio = 1.0/3.0;
#endif

  Real alpha = height_limit * 2.0 / pi;
  Real beta = pi * ratio / (2.0 * height_limit);

  Offset delta (encompass_[encompass_.size () - 1].x () 
    - encompass_[0].x (), 0);
  Real b = delta.length ();
  Real indent = alpha * atan (beta * b);
  Real height = indent + h;
 
#define RESIZE_ICE
#ifndef RESIZE_ICE
  Array<Offset> control;
  control.push (Offset (0, 0));
  control.push (Offset (indent, height));
  control.push (Offset (b - indent, height));
  control.push (Offset (b, 0));
#else
  Array<Offset> control (4);
  control[0] = Offset (0, 0);
  control[1] = Offset (indent, height);
  control[2] = Offset (b - indent, height);
  control[3] = Offset (b, 0);
#endif
  Bezier::set (control);
}

