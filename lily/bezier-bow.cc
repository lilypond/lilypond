/*
  bezier.cc -- implement Bezier and Bezier_bow

  source file of the GNU LilyPond music typesetter

  (c) 1998--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <math.h>

#include "bezier-bow.hh"
#include "misc.hh"
#include "bezier.hh"


static Real
F0_1 (Real x)
{
  return 2 / M_PI * atan ( M_PI * x / 2);
}

Real
slur_height (Real width, Real h_inf, Real r_0)
{
  return F0_1 (width * r_0 / h_inf) * h_inf;
}

  /*
  For small w, the height should be proportional to w, for w ->
  infinity, the height should rise to a limit asymptotically.

  Hence we take F(x) such that
  F(0) = 0 , F'(0) = 1, and F(infty) = 1

  and use

  h = h_infinity * F(x * r_0 / h_infinity)

  
  Examples:

  * F(x) = 2/pi * atan (pi x/2)

  * F(x) 1/alpha * x^alpha / (1 + x^alpha)

  * (etc.)

  [with the 2nd recipe you can determine how quickly the conversion from
  `small' slurs to `big' slurs occurs.]

  Although this might seem cand_idates to SCM-ify, it is not all clear
  which parameters (ie. h_inf, r_0, F(.)) should be candidates for
  this.  At present h_inf and r_0 come from paper settings, but we did
  no experiments for determining the best combinations of F, h_inf and
  r_0.

  */

Bezier
slur_shape (Real width, Real h_inf, Real r_0)
{
  Bezier curve;
  Real height =  slur_height (width, h_inf, r_0);
  Real indent = height;

  curve.control_[0] = Offset (0, 0);
  curve.control_[1] = Offset (indent, height);
  curve.control_[2] = Offset (width - indent, height);
  curve.control_[3] = Offset (width, 0);
  return curve;
}

