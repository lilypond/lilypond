/*
  bezier.cc -- implement Bezier and Bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include <math.h>
#include "offset.hh"
#include "bezier.hh"
#include "dimen.hh"
#include "paper-def.hh"

Bezier::Bezier (int steps_i)
{
  steps_i_ = steps_i;
  curve_ = new Offset [steps_i_ + 1];
}

Bezier::~Bezier ()
{
  delete[] curve_;
}

//from GNU gs3.33: ega.c
void
Bezier::calc (Offset control[4])
{       
  Real dt = 1.0 / steps_i_;
  Real cx = 3.0 * (control[1].x() - control[0].x());
  Real bx = 3.0 * (control[2].x() - control[1].x()) - cx;
  Real ax = control[3].x() - (control[0].x() + cx + bx);
  Real cy = 3.0 * (control[1].y () - control[0].y ());
  Real by = 3.0 * (control[2].y () - control[1].y ()) - cy; 
  Real ay = control[3].y () - (control[0].y () + cy + by);
  Real t = 0.0;
  int i = 0;
  while ( t <= 1.0 )
    {    
      curve_[i].x() = ((ax * t + bx) * t + cx) * t + control[0].x();
      curve_[i++].y () = ((ay * t + by) * t + cy) * t + control[0].y ();
      t += dt;
    }
}

Real
Bezier::y (Real x)
{
  if (x <= curve_[0].x())
    return curve_[0].y ();
  for (int i = 1; i < steps_i_; i++ )
    {
      if (x < curve_[i].x())
	{
	  Real lin = (x - curve_[i-1].x()) / (curve_[i].x() - curve_[i-1].x());
	  return curve_[i-1].y () + lin * (curve_[i].y () - curve_[i-1].y ());
        }
    }
  return curve_[steps_i_-1].y ();
}


Bezier_bow::Bezier_bow (Paper_def* paper_l)
  : Bezier(10)
{
  paper_l_ = paper_l;
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
Bezier_bow::calc (Real dx, Real dy, Real h, Real d)
{
  // ugh
  Real pi = M_PI;
  // ugh
  Real staffsize_f = paper_l_->get_var ("barsize");
  Real height_limit = staffsize_f;
  Real alpha = height_limit * pi / 2.0;
  Real ratio = 1.0/3.0;
  Real beta = 3.0/4.0 * pi * ratio/alpha;

  Real b = sqrt (dx * dx + dy * dy);
  Real indent = 2.0/5.0 * alpha * atan (beta * b);
  // ugh, ugly height hack, see lily-ps-defs.tex
  Real height = (indent + h) * d;
 
  Offset control[4];
  control[0] = Offset(0, 0);
  control[1] = Offset(indent, height);
  control[2] = Offset(b - indent, height);
  control[3] = Offset( b, 0 );
 
  Real phi = dx ? atan (dy/dx) : sign (dy) * pi / 2.0;
  Real sphi = sin (phi);
  Real cphi = cos (phi);
  for (int i = 1; i < 4; i++)
    {
      control[i].x() = cphi * control[i].x() - sphi * control[i].y ();
      control[i].y () = sphi * control[i].x() + cphi * control[i].y ();
    }
  Bezier::calc (control);
}

