/*
  bezier.cc -- implement Bezier and Bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>
#include "bezier-bow.hh"
#include "misc.hh"
#include "bezier.hh"
#include "dimensions.hh"
#include "direction.hh"
#include "debug.hh"
#include "main.hh"
#include "lily-guile.hh"

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


Bezier_bow::Bezier_bow (Array<Offset> points, Direction dir)
{
  dir_ = dir;
  encompass_ = points;
  to_canonic_form ();

  rc_factor_ = 1.0;
  height_limit_ = 1.0;
  ratio_ = 1.0;
}

static Real
default_height (Real len)
{
  // assume 20pt staff
  // see fonts.doc
  Real staff_space = 5.0;
  Real h_inf = 2.0* staff_space;
  Real r_0 = 0.33;
  return h_inf * 2.0 / M_PI * atan ( M_PI * r_0 / (2.0 * h_inf) * len);
}

void
Bezier_bow::blow_fit ()
{
  Real len = curve_.control_[3][X_AXIS]; 
  Real h = curve_.control_[1][Y_AXIS] * fit_factor () / len;
  curve_.control_[1][Y_AXIS] = h * len;
  curve_.control_[2][Y_AXIS] = h * len;  
  curve_.check_sanity ();
}

void
Bezier_bow::de_uglyfy ()
{
  Real len = curve_.control_[3][X_AXIS] ; 
  Real ff = fit_factor ();
  for (int i = 1; i < 3; i++)
    {
      Real ind = abs (curve_.control_[(i-1)*3][X_AXIS]
		      - curve_.control_[i][X_AXIS]) / len;
      Real h = curve_.control_[i][Y_AXIS] * ff / len;

      // ugh. Unhardcode this
#if 0
      // Too crude.
      if (h > 4 * ind)
	{
	  h = 4* ind; 
	}
#else
      Real f = default_height (len) / len;
      if (h > 2.0 * f)
	{
	  h = 2.0 * f; 
	}
#endif
      
      if (h > 0.8 + -2 * ind)
	{
	  h = 0.8 - 2  *ind; 
	}
      
      curve_.control_[i][Y_AXIS] = h * len;  
    } 

  curve_.check_sanity ();
}

Real
Bezier_bow::calc_enclosed_area_f () const
{
  Real a = 0;
  for (int i=0; i < encompass_.size (); i++)
    {
      Interval x;
      Interval y;
      if (i == 0)
	{
	  x = Interval (0, encompass_[1][X_AXIS] / 2);
	  y = Interval (0,
			curve_.get_other_coordinate (X_AXIS,
						     encompass_[1][X_AXIS]
						     / 2));
	}
      else if (i == encompass_.size () - 1)
	{
	  x = Interval ((encompass_[i-1][X_AXIS] + encompass_[i][X_AXIS])/2, 
			encompass_[i][X_AXIS]);
	  y = Interval (0,
			(curve_.get_other_coordinate (X_AXIS,
						      (x[MIN] + x[MAX]) / 2)));
	}
      else
	{
	  x = Interval ((encompass_[i-1][X_AXIS] + encompass_[i][X_AXIS]) / 2, 
			(encompass_[i][X_AXIS] + encompass_[i+1][X_AXIS]) / 2);
	  y = Interval (encompass_[i][Y_AXIS],
			(curve_.get_other_coordinate (X_AXIS, x[MIN])
			 + curve_.get_other_coordinate (X_AXIS,
							(x[MIN] + x[MAX]) / 2)
			 + curve_.get_other_coordinate (X_AXIS, x[MAX])) / 3);
	}
      
      Real da = x.length () * y.length ();
      a += da;
    }
  return a;
}

Array<Offset>
Bezier_bow::area_gradient_offset_arr ()
{
  Real len = curve_.control_[3][X_AXIS]; 
  Real area = calc_enclosed_area_f ();
  
  Real grow = len / 10.0;
  Array<Offset> da (2);
  for (int i=1; i < 3; i++)
    {
      for (Axis a=X_AXIS; a < NO_AXES; incr (a)) 
	{
	  Real r = curve_.control_[i][a];
	  curve_.control_[i][a] += grow;
	  da[i-1][a] = (calc_enclosed_area_f () - area) / grow;
	  
	  curve_.control_[i][a] = r;
	}
    }
  return da;
}

void
Bezier_bow::minimise_enclosed_area ()
{
  Real len = curve_.control_[3][X_AXIS]; 
  Real beautiful = len * default_height (len) / 2.0;

  DEBUG_OUT << to_str ("Beautiful: %f\n", beautiful);
  DEBUG_OUT << to_str ("Length: %f\n", len);
  int steps=2;
  for (int i=0; i < steps; i++)
    {
      Real ff = fit_factor ();
      if (!ff)
	break;

      DEBUG_OUT << to_str ("FitFac: %f\n", ff);

      // slur must be higher at every point
      if (ff > 1.01)
	{
	  blow_fit ();
	  DEBUG_OUT << to_str ("Blown area: %f\n", calc_enclosed_area_f ());
	}
      else
	  DEBUG_OUT << to_str ("Init area: %f\n", calc_enclosed_area_f ());

      Real area = calc_enclosed_area_f ();
      

      if (area <= beautiful)
	break;

      Array<Offset> da = area_gradient_offset_arr ();

      /*
	Urg: empiric cs
	     Small slurs are easily too asymmetric,
	     while big slurs are too symmetric

	     This makes short slurs strictly x-bound,
	     long slurs become y-bound.
       */
      Real ypct = 0.50;
      //Real xpct = (0.07 * len * len / 1000.0) <? 0.80;
      Real xpct = (0.1 * len * len * len / 100000.0) <? 0.80;

      Real yu = (abs (curve_.control_[1][Y_AXIS] / da[0][Y_AXIS])
		 <? abs (curve_.control_[2][Y_AXIS] / da[1][Y_AXIS]))
	* ypct;
      Real xu = (abs (curve_.control_[1][X_AXIS] / da[0][X_AXIS])
		 <? abs ((curve_.control_[3][X_AXIS]
			     - curve_.control_[2][X_AXIS]) / da[1][X_AXIS]))
	* xpct;
      Real u = yu <? xu;
      DEBUG_OUT << to_str ("u (xu, yu): %f (%f, %f)\n", u, xu, yu);
      DEBUG_OUT << to_str ("pct (x, y): (%f, %f)\n", xpct, ypct);

      DEBUG_OUT << to_str ("da1: (%f, %f)\n", da[0][X_AXIS], da[0][Y_AXIS]);
      DEBUG_OUT << to_str ("da2: (%f, %f)\n", da[1][X_AXIS], da[1][Y_AXIS]);

      curve_.control_[1] -= da[0] * u;
      curve_.control_[2] -= da[1] * u;
    }
  

  if (fit_factor () > 1.5)
    blow_fit ();

  DEBUG_OUT << to_str ("Exarea: %f\n", calc_enclosed_area_f ());
  Real area = calc_enclosed_area_f ();
  /*
    Slurs that fit beautifully are not ugly
   */
  if (area > beautiful)
    {
      DEBUG_OUT << "DE-UGLYFY\n";
      de_uglyfy ();
    }

}

void
Bezier_bow::calculate ()
{
  calc_default ();
  if (fit_factor () > 1.0)
    {
      //    calc_tangent_controls ();
      // blow_fit ();
      minimise_enclosed_area ();
    }
}


  
Bezier
Bezier_bow::get_curve ()const
{
  Bezier rv = curve_;
  if (dir_ == DOWN)
    {
      rv.flip (Y_AXIS);
    }

  rv.rotate (alpha_);
  rv.translate (origin_);
  
  return rv;
}

static Real const FUDGE = 1e-8;

/*
  This function calculates 2 center control points,
  based on lines through c_0 --> left disturbing
  and c_3--> right disturbing encompass points.
  
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


  Drul_array<Real> angles;
  Direction d = LEFT;
  do
    {
      maxtan[d] *= -d * rc_factor_;
      angles[d] = atan (maxtan[d]);
    }
  while (flip(&d) != LEFT);

  Real rc3 = 0.0;

  /* 
    if we have two disturbing points, have line through those...
    in order to get a sane line, make sure points are reasonably far apart
    X distance must be reasonably(!) big (division)
   */
  if (abs (disturb[LEFT][X_AXIS] - disturb[RIGHT][X_AXIS]) > FUDGE)
    rc3 = (disturb[RIGHT][Y_AXIS] - disturb[LEFT][Y_AXIS]) / (disturb[RIGHT][X_AXIS] - disturb[LEFT][X_AXIS]);

  else
    rc3 = tan ((angles[LEFT] - angles[RIGHT]) / 2);


  // ugh: be less steep
  rc3 /= 2*rc_factor_;
  

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
}

/*
  max ( encompass.y / curve.y )
  
 */
Real
Bezier_bow::fit_factor () const
{
  Real x1 = encompass_[0][X_AXIS];
  Real x2 = encompass_.top ()[X_AXIS];

  Real factor = 0.0;
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


  return factor;
}




void
Bezier_bow::to_canonic_form ()
{
  origin_ = encompass_[0];
  translate (encompass_,-origin_);

  Offset delta = encompass_.top () - encompass_[0];
  alpha_ = delta.arg ();

  rotate (encompass_, -alpha_);
  if (dir_ == DOWN)
    {
      flipy (encompass_);
    }

  while (encompass_.size () > 1 && encompass_[1][X_AXIS] <= 0.0)
    {
      programming_error ("Degenerate slur: infinite steepness reqd");
      encompass_.del (1);
    }

  Real l = encompass_.top ()[X_AXIS];
  while (encompass_.size () > 1 && encompass_.top (1)[X_AXIS] >= l)
    {
      programming_error ("Degenerate slur: infinite steepness reqd");
      encompass_.del (encompass_.size ()-2);
    }
}



/*
 See Documentation/fonts.tex
 */
void
Bezier_bow::calc_default ()
{
  Real pi = M_PI;

  Real alpha = height_limit_ * 2.0 / pi;
  Real beta = pi * ratio_ / (2.0 * height_limit_);

  Offset delta (encompass_.top ()[X_AXIS] 
    - encompass_[0][X_AXIS], 0);

  Real b = delta.length ();
  Real indent = alpha * atan (beta * b);
  Real height = indent;
 
  curve_.control_ [0] = Offset (0, 0);
  curve_.control_ [1] = Offset (indent, height);
  curve_.control_ [2] = Offset (b - indent, height);
  curve_.control_ [3] = Offset (b, 0);
}




