/*
  tie-helper.cc -- implement Tie_configuration, Tie_details

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "tie.hh"
#include "bezier.hh"
#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "warn.hh"
#include "tie-formatting-problem.hh"

int
Tie_configuration::compare (Tie_configuration const &a,
			    Tie_configuration const &b)
{
  if (a.position_ - b.position_)
    return sign (a.position_ - b.position_);
  return sign (a.dir_ - b.dir_);
}
			    

Tie_configuration::Tie_configuration ()
{
  dir_ = CENTER;
  position_ = 0;
  delta_y_ = 0.0;
}


void
Tie_configuration::center_tie_vertically (Tie_details const &details)
{
  Bezier b = get_bezier (details);
  Offset middle = b.curve_point (0.5);
  Offset edge = b.curve_point (0.0);

  Real center = (edge[Y_AXIS] + middle[Y_AXIS])/2.0;

  delta_y_ = - dir_ * center;
}


/*
  Get bezier with left control at (0,0)
 */
Bezier
Tie_configuration::get_bezier (Tie_details const &details) const
{
  Real l = attachment_x_.length();
  if (isnan (l) || isnan (l))
    {
      programming_error ("Inf or NaN encountered");
      l = 1.0;
    }
  return slur_shape (l,
		     details.height_limit_,
		     details.ratio_);
}

Real
Tie_configuration::distance (Tie_configuration const &a,
			     Tie_configuration const &b)
{

  Real d = 3 * (a.position_ - b.position_);
  if (d < 0)
    return d + (2 + (b.dir_ - a.dir_));
  else
    return d + (2 + (a.dir_ - b.dir_));
}

Real
Tie_configuration::height (Tie_details const &details) const
{
  Real l = attachment_x_.length();

  return slur_shape (l,
		     details.height_limit_,
		     details.ratio_).curve_point (0.5)[Y_AXIS]; 
}



void
Tie_details::from_grob (Grob *me)
{
  staff_space_ = Staff_symbol_referencer::staff_space (me);
  SCM details = me->get_property ("details");


  height_limit_ = robust_scm2double (ly_assoc_get (ly_symbol2scm ("height-limit"), details, SCM_EOL),
				     0.75) * staff_space_;
  
  ratio_ = robust_scm2double (ly_assoc_get (ly_symbol2scm ("ratio"), details, SCM_EOL),
			      .333);
  
  x_gap_ = robust_scm2double (me->get_property ("x-gap"), 0.2);
  between_length_limit_
    = robust_scm2double (ly_assoc_get (ly_symbol2scm ("between-length-limit"), details, SCM_EOL),
			 1.0); 
  
}

Tie_details::Tie_details ()
{
  staff_space_ = 1.0; 
  height_limit_ = 1.0;
  ratio_ = .333;   
}

