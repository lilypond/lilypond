/*
  slur-bezier-bow.cc -- implement Slur_bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 2000  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "debug.hh"
#include "paper-def.hh"
#include "slur-bezier-bow.hh"
#include "main.hh"

Slur_bezier_bow::Slur_bezier_bow (Array<Offset> encompass, Direction dir)
  : Bezier_bow (encompass, dir)
{
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

void
Slur_bezier_bow::minimise_enclosed_area (Paper_def* paper_l,
					 Real default_height)
{
  Real length = curve_.control_[3][X_AXIS]; 
  Real sb = paper_l->get_var ("slur_beautiful");
  Real beautiful = length * default_height * sb;

  DEBUG_OUT << to_str ("Beautiful: %f\n", beautiful);
  DEBUG_OUT << to_str ("Length: %f\n", length);
  DEBUG_OUT << to_str ("D-height: %f\n", default_height);
  DEBUG_OUT << to_str ("FitFac: %f\n", fit_factor ());

  if (fit_factor () > 1.0)
    blow_fit ();
  
  Real pct_c0 = paper_l->get_var ("bezier_pct_c0");
  Real pct_c3 = paper_l->get_var ("bezier_pct_c3");
  Real pct_in_max = paper_l->get_var ("bezier_pct_in_max");
  Real pct_out_max = paper_l->get_var ("bezier_pct_out_max");
  Real steps = paper_l->get_var ("bezier_area_steps");

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

      DEBUG_OUT << to_str ("pct: %f\n", pct);
      DEBUG_OUT << to_str ("u: %f\n", u);

      DEBUG_OUT << to_str ("da: (%f, %f)\n", da[0], da[1]);
      DEBUG_OUT << to_str ("da*u: (%f, %f)\n", da[0]*u*pct, da[1]*u*pct);
      DEBUG_OUT << to_str ("cx: (%f, %f)\n", curve_.control_[1][X_AXIS],
			   curve_.control_[2][X_AXIS]);

      curve_.control_[1][X_AXIS] -= da[0] * u * pct;
      curve_.control_[2][X_AXIS] -= da[1] * u * pct;
    }

  Real area = enclosed_area_f ();
  DEBUG_OUT << to_str ("Exarea: %f\n", area);
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




