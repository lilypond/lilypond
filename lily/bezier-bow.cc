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
#include "paper-def.hh"


Bezier_bow::Bezier_bow (Array<Offset> encompass, Direction dir)
{
  alpha_ = 0;
  dir_ = dir;
  encompass_ = encompass;
  to_canonical_form ();
}

Bezier
Bezier_bow::get_bezier () const
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

void
Bezier_bow::to_canonical_form ()
{
  origin_ = encompass_[0];
  translate (&encompass_, -origin_);

  Offset delta = encompass_.top () - encompass_[0];
  alpha_ = delta.arg ();

  rotate (&encompass_, -alpha_);
  if (dir_ == DOWN)
    {
      flip (&encompass_, Y_AXIS);
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
Bezier_bow::set_default_bezier (Real h_inf, Real r_0)
{
  curve_ = get_default_bezier (h_inf, r_0);
}

/*
  See Documentation/programmer/fonts.doc
 */
Bezier
Bezier_bow::get_default_bezier (Real h_inf, Real r_0) const
{
  Offset delta (encompass_.top ()[X_AXIS] - encompass_[0][X_AXIS], 0);
  Real b = delta.length ();
  Real height = get_default_height (h_inf, r_0, b);
  // urg: scmify this?
  Real indent = height;

  Bezier curve;
  curve.control_[0] = Offset (0, 0);
  curve.control_[1] = Offset (indent, height);
  curve.control_[2] = Offset (b - indent, height);
  curve.control_[3] = Offset (b, 0);
  return curve;
}

/*
  See Documentation/programmer/fonts.doc
 */
Real
Bezier_bow::get_default_height (Real h_inf, Real r_0, Real b) const
{

  SCM h = scm_eval (scm_listify (ly_symbol2scm ("slur-default-height"),
				 gh_double2scm (h_inf),
				 gh_double2scm (r_0),
				 gh_double2scm (b),
				 SCM_UNDEFINED));
  return gh_scm2double (h);
}
  
