/*
  bezier.hh -- declare Bezier and Bezier_bow

  (c) 1998--1999 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef BEZIER_HH
#define BEZIER_HH

#ifndef STANDALONE
#include "lily-proto.hh"
#endif

#include "real.hh"
#include "curve.hh"
#include "drul-array.hh"
#include "interval.hh"

/**
  Simple bezier curve
 */
class Bezier
{
public:
  Bezier ();

  /**
  Calculate bezier curve into Offset (x,y) array.
  */
  void calc (int steps);
  void print () const;

  void set (Array<Offset> points);

  /**
  Return y that goes with x by interpolation.
  */
  Real y (Real x);

  Curve curve_;
  Curve control_;
};

/**
  Implement bow specific bezier curve
 */
class Bezier_bow : public Bezier
{
public:
  Bezier_bow (Paper_def* paper_l);

  /**
   Calculate bezier curve for bow from bow paratime_signatures.
   */
  void blow_fit ();
  void calc ();
  Real calc_f (Real height);
  void calc_bezier ();
  bool calc_clipping ();
  void calc_controls ();
  void calc_default (Real h);
  void calc_return (Real begin_alpha, Real end_alpha);
  void calc_tangent_controls ();
  bool check_fit_bo ();
  Real check_fit_f ();
  void print () const;
  void set (Array<Offset> points, Direction dir);
  void transform ();
  void transform_back ();

  Paper_def* paper_l_;
  Curve encompass_;
  Direction dir_;
  void set_direction (Direction d ) { dir_ =  d; }
  Direction get_direction () const { return dir_; }

  Real alpha_;
  Offset origin_;
  Curve return_;
  Drul_array<Interval> curve_extent_drul_;
};


#endif // BEZIER_HH

