/*
  slur-bezier-bow.cc -- implement Slur_bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 2000  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "debug.hh"
#include "paper-def.hh"
#include "slur-bezier-bow.hh"
#include "main.hh"

Slur_bezier_bow::Slur_bezier_bow (Array<Offset> encompass, Direction dir,
				  Real h_inf, Real r_0)
{
  h_inf_ = h_inf;
  r_0_ = r_0;
  alpha_ = 0;
  dir_ = dir;
  encompass_ = encompass;
  to_canonical_form ();

  Real w = encompass_.top ()[X_AXIS] - encompass_[0][X_AXIS];
  curve_ = slur_shape (w, h_inf, r_0);
}

Bezier
Slur_bezier_bow::get_bezier () const
{
  Bezier rv = curve_;
  if (dir_ == DOWN)
    {
      rv.scale (1, -1);
    }

  rv.rotate (alpha_);
  rv.translate (origin_);
  
  return rv;
}

void
Slur_bezier_bow::to_canonical_form ()
{
  origin_ = encompass_[0];
  translate (&encompass_, -origin_);

  Offset delta = encompass_.top () - encompass_[0];
  alpha_ = delta.arg ();

  rotate (&encompass_, -alpha_);
  if (dir_ == DOWN)
    {
      scale (&encompass_, 1, -1);
    }

  while (encompass_.size () > 1 && encompass_[1][X_AXIS] <= 0.0)
    {
      programming_error ("Degenerate bow: infinite steepness reqd");
      encompass_.del (1);
    }

  Real l = encompass_.top ()[X_AXIS];
  while (encompass_.size () > 1 && encompass_.top (1)[X_AXIS] >= l)
    {
      programming_error ("Degenerate bow: infinite steepness reqd");
      encompass_.del (encompass_.size ()-2);
    }
}



void
Slur_bezier_bow::blow_fit ()
{
  Real len = curve_.control_[3][X_AXIS]; 
  Real h = curve_.control_[1][Y_AXIS] * fit_factor () / len;
  curve_.control_[1][Y_AXIS] = h * len;
  curve_.control_[2][Y_AXIS] = h * len;  
  curve_.assert_sanity ();
}


Real
Slur_bezier_bow::enclosed_area_f () const
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

Array<Real>
Slur_bezier_bow::area_x_gradients_array (Real area)
{
  Real len = curve_.control_[3][X_AXIS]; 
  Real grow = len / 10.0;
  Array<Real> da (2);
  for (int i=0; i < 2; i++)
    {
      Real r = curve_.control_[i+1][X_AXIS];
      curve_.control_[i+1][X_AXIS] += grow;
      da[i] = (enclosed_area_f () - area) / grow;
      curve_.control_[i+1][X_AXIS] = r; 
    }
  return da;
}

/*
  ugh, should have another look, and use a regular optimization
  algorithm, instead of this homebrew.
*/
void
Slur_bezier_bow::minimise_enclosed_area (Real beauty,
					 SCM bezier_props)
{
  Real length = curve_.control_[3][X_AXIS]; 
  Real beautiful = beauty * length * slur_height (length, h_inf_, r_0_);


  if (fit_factor () > 1.0)
    blow_fit ();
  
  Real pct_c0 = gh_scm2double (gh_cdr (scm_assoc (ly_symbol2scm ("bezier-pct-c0"), bezier_props)));
  Real pct_c3 = gh_scm2double (gh_cdr (scm_assoc (ly_symbol2scm ("bezier-pct-c3"), bezier_props)));
  Real pct_in_max =  gh_scm2double (gh_cdr (scm_assoc (ly_symbol2scm ("bezier-pct-in-max"), bezier_props)));
  Real pct_out_max = gh_scm2double (gh_cdr (scm_assoc (ly_symbol2scm ("bezier-pct-out-max"), bezier_props)));
  Real steps =  gh_scm2double (gh_cdr (scm_assoc (ly_symbol2scm ("bezier-area-steps"),bezier_props)));

  for (int i=0; i < steps; i++)
    {
      Real area = enclosed_area_f ();
      if (!i)
	DEBUG_OUT << to_str ("Init area: %f\n", area);

      if (area <= beautiful)
	break;

      Array<Real> da = area_x_gradients_array (area);

      // urg
      Real pct = pct_c0 + pct_c3 * length * length * length;
      pct *= (steps - i) / steps;
      if (da[0] > 0 || da[1] < 0)
	pct = pct <? pct_out_max;
      else
	pct = pct <? pct_in_max;

      Real u = (abs (curve_.control_[1][X_AXIS] / da[0])
		<? abs ((curve_.control_[3][X_AXIS]
			 - curve_.control_[2][X_AXIS]) / da[1]));

      curve_.control_[1][X_AXIS] -= da[0] * u * pct;
      curve_.control_[2][X_AXIS] -= da[1] * u * pct;
    }

  //  Real area = enclosed_area_f ();
}



/*
  max ( encompass.y / curve.y )
  
 */
Real
Slur_bezier_bow::fit_factor () const
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




