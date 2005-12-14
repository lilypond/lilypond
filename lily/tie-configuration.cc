/*
  tie-configuration.cc -- implement Tie_configuration

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "tie-configuration.hh"
#include "warn.hh"
#include "tie-formatting-problem.hh"
#include "bezier.hh"

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
  Bezier b = get_untransformed_bezier (details);
  Offset middle = b.curve_point (0.5);
  Offset edge = b.curve_point (0.0);
  Real center = (edge[Y_AXIS] + middle[Y_AXIS])/2.0;

  delta_y_ = - dir_ * center;
}


Bezier
Tie_configuration::get_transformed_bezier (Tie_details const &details) const
{
  Bezier b (get_untransformed_bezier (details));

  b.scale (1, dir_);
  b.translate (Offset (attachment_x_[LEFT],
		       delta_y_ + details.staff_space_ * 0.5 * position_));

  return b;
}

/*
  Get bezier with left control at (0,0)
 */
Bezier
Tie_configuration::get_untransformed_bezier (Tie_details const &details) const
{
  Real l = attachment_x_.length();
  if (isinf (l) || isnan (l))
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
