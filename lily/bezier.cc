/*
  bezier.cc -- implement Bezier and Bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include <math.h>
#include "bezier.hh"
#include "direction.hh"

#ifndef STANDALONE
#include "direction.hh"
#include "dimen.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "main.hh"
#define SLUR_DOUT if (check_debug && !monitor->silent_b ("Slur")) cout
#else
#define SLUR_DOUT cerr
#endif

void
Curve::flipy ()
{
  // ugh, Offset should have mirror funcs
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
  Offset rot (complex_exp (Offset (0,phi)));

  for (int i = 1; i < size (); i++)
    (*this)[i] = complex_multiply (rot, (*this)[i]);
}

void
Curve::translate (Offset o)
{
  for (int i = 1; i < size (); i++)
    (*this)[i] += o;
}

Bezier::Bezier (int steps)
{
  control_.set_size (4);
  curve_.set_size (steps);
}

//from GNU gs3.33: ega.c
void
Bezier::calc ()
{       
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
  if (x <= curve_[0].x ())
    return curve_[0].y ();
  for (int i = 1; i < curve_.size (); i++ )
    {
      if (x < curve_[i].x ())
	//           ^ ? see below   
	{
	  Real lin = (x - curve_[i-1].x ()) / (curve_[i] - curve_[i-1]).x ();
	  //                     ^ ?
	  return (curve_[i-1] + lin * (curve_[i] - curve_[i-1])).y ();
        }
    }
  return curve_[curve_.size ()-1].y ();
}


Bezier_bow::Bezier_bow (Paper_def* paper_l)
  : Bezier(10)
{
  paper_l_ = paper_l;
  return_.set_size (4);
}

/* 
  from feta-sleur.ly

	slurheightlimit#:=staffsize#/2;
	sluralpha:=slurheightlimit#*pi/2;
	% slurratio:=1/3;
	slurratio:=0.3333;
	slurbeta:=3/4*pi*slurratio/sluralpha;

        b#:=length(dx#,dy#);
        % ugh: huh? 2/5
        indent#:=2/5*sluralpha*atan(slurbeta*b#);
        height:=(indent+h)*d;
        z1=(0,0);
        z2=(b,0);
        z3=(indent,height);
        z4=(b-indent,height);

	boogje:=boogje rotated angle(dxs,dys);
*/

void
Bezier_bow::blow_fit ()
{
  Real dy1 = check_fit_f ();
  if (!dy1)
    return;

  // be careful not to take too big step
  Real f = 0.75;
  Real h1 = dy1 * f;
  control_[1].y () += h1; 
  control_[2].y () += h1; 
  return_[1].y () += h1; 
  return_[2].y () += h1; 

  Real dy2 = check_fit_f ();
  if (!dy2)
    return;

#ifndef STANDALONE
  Real epsilon = paper_l_->rule_thickness ();
#else
  Real epsilon = 1.5 * 0.4 PT;
#endif
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

  control_[1].y () += -h1 +h; 
  control_[2].y () = -h1 +h; 
  return_[1].y () = -h1 +h;
  return_[2].y () = -h1 +h; 
}

Real
Bezier_bow::calc_f (Real height)
{
  transform ();
  calc_default (height);
  Bezier::calc ();
  
  Real dy = check_fit_f ();
  calc_return (0, 0);

  transform_controls_back ();
  return dy;
}

void
Bezier_bow::calc ()
{
  transform ();
  calc_default (0);
  Bezier::calc ();
  
  if (check_fit_bo ())
    calc_return (0, 0);
  else
    {
      calc_controls ();
      blow_fit ();
    }

  transform_controls_back ();
}

void
Bezier_bow::calc_return (Real begin_alpha, Real end_alpha)
{
#ifndef STANDALONE
  Real thick = 1.8 * paper_l_->rule_thickness ();
#else
  Real thick = 10.0 * 1.8 * 0.4 PT;
#endif
  return_[0] = control_[3];

  return_[1] = control_[2] - thick * complex_exp (Offset (0, 90 + end_alpha));
  return_[2] = control_[1] - thick * complex_exp (Offset (0, 90 - begin_alpha));  
  
  /*
  return_[1].x () = control_[2].x () - thick * cos (90 + end_alpha);
  return_[1].y () = control_[2].y () - thick * sin (90 + end_alpha);
  return_[2].x () = control_[1].x () - thick * cos (90 - begin_alpha);
  return_[2].y () = control_[1].y () - thick * sin (90 - begin_alpha);
  */
  return_[3] = control_[0];
}

void
Bezier_bow::calc_controls ()
{
  // ugh: tooo steep
//  Real default_rc = atan (control_[1].y () / control_[1].x ());
  
  Offset ijk_p (control_[3].x () / 2, control_[1].y ());
  SLUR_DOUT << "ijk: " << ijk_p.x () << ", " << ijk_p.y () << endl;

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

  // ugh
  Curve reversed;
  reversed.set_size (encompass_.size ());
  Real b = control_[3].x ();
  for (int i = 0; i < encompass_.size (); i++ )
    {
      reversed[i] = Offset (b,0) - encompass_[encompass_.size () - i -1];
      /*
      reversed[i].x () = b - encompass_[encompass_.size () - i - 1].x ();
      reversed[i].y () = encompass_[encompass_.size () - i - 1].y ();
      */
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
  SLUR_DOUT << "begin " << begin_p.x () << ", " << begin_p.y () << endl;
  SLUR_DOUT << "end " << end_p.x () << ", " << end_p.y () << endl;

  Real height =control_[1].y (); 
  for (int i = 0; i < encompass_.size (); i++ )
    height = height >? encompass_[i].y ();

  // emperic computer science:
  //   * tangents somewhat steeper than minimal line
  Real rc_correct = 2.4;

  begin_rc *= rc_correct;
  end_rc *= rc_correct;
  Real rc1 = begin_rc;
  Real rc2 = -end_rc;
  
  Real begin_alpha = atan (begin_rc);
  Real end_alpha = atan (-end_rc);
  Real theta = (begin_alpha - end_alpha) / 2;

  // if we have two disturbing points, have height line through those...
  /*
    UGH UGH UGH! NEVER compare floats with == 
   */
  if (!((begin_p.x () == end_p.x ()) && (begin_p.y () == end_p.y ())))
      theta = atan (end_p.y () - begin_p.y ()) / (end_p.x () - begin_p.x ());

  Real rc3 = tan (theta);
  // ugh: be less steep
  rc3 /= 2*rc_correct;

  Real c2 = -rc2 * control_[3].x ();
  Real c3 = begin_p.y () > end_p.y () ? begin_p.y () 
    - rc3 * begin_p.x () : end_p.y () - rc3 * end_p.x ();

  SLUR_DOUT << "y1 = " << rc1 << " x + 0" << endl;
  SLUR_DOUT << "y2 = " << rc2 << " x + " << c2 << endl;
  SLUR_DOUT << "y3 = " << rc3 << " x + " << c3 << endl;
  control_[1].x () = c3 / (rc1 - rc3);
  control_[1].y () = rc1 * control_[1].x ();
  control_[2].x () = (c3 - c2) / (rc2 - rc3);
  SLUR_DOUT << "c2.x () = " << control_[2].x () << endl;
  SLUR_DOUT << "(c3 - c2) = " << (c3 - c2) << endl;
  SLUR_DOUT << "(rc2 - rc3) = " << (rc2 - rc3) << endl;
  control_[2].y () = rc2 * control_[2].x () + c2;
  SLUR_DOUT << "c2.y ()" << control_[2].y () << endl;

  calc_return (begin_alpha, end_alpha);
}

bool
Bezier_bow::check_fit_bo ()
{
  for (int i = 1; i < encompass_.size () - 1; i++)
    if (encompass_[i].y () > y (encompass_[i].x ()))
      return false;
  return true;
}

Real
Bezier_bow::check_fit_f ()
{
  Real dy = 0;
  for (int i = 1; i < encompass_.size () - 1; i++)
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
  /*
  Real dx = encompass_[encompass_.size () - 1].x () - encompass_[0].x (); 
  Real dy = encompass_[encompass_.size () - 1].y () - encompass_[0].y ();
  */

  alpha_ = delta.arg ();
  encompass_.rotate (-alpha_);

  if (dir_ == DOWN)
    encompass_.flipy ();
}

void
Bezier_bow::transform_controls_back ()
{
  // silly name; let's transform encompass back too
  // to allow recalculation without re-set()ting encompass array
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

void
Bezier_bow::calc_default (Real h)
{
  Real pi = M_PI;
#ifndef STANDALONE
  Real staffsize_f = paper_l_->get_var ("barsize");
#else
  Real staffsize_f = 16 PT;
#endif

  Real height_limit = staffsize_f;
  Real alpha = height_limit * pi / 2.0;
  Real ratio = 1.0/3.0;
  Real beta = 3.0/4.0 * pi * ratio/alpha;


  Offset delta (encompass_[encompass_.size () - 1].x () - encompass_[0].x (), 0);

  Real d = 1;

  Real b = delta.length ();
  Real indent = 2.0/5.0 * alpha * atan (beta * b);
  // ugh, ugly height hack, see lily-ps-defs.tex
  Real height = (indent + h) * d;
 
//  Offset control[4] = {0, 0, indent, height, b - indent, height, b, 0 };
  Array<Offset> control;
  control.push (Offset (0, 0));
  control.push (Offset (indent, height));
  control.push (Offset (b - indent, height));
  control.push (Offset (b, 0));
  Bezier::set (control);
 
//  Real phi = dx ? atan (dy/dx) : sign (dy) * pi / 2.0;
//  control.rotate (phi);
}

