/*
  bezier.hh -- declare Bezier and Bezier_bow

  (c) 1998 Jan Nieuwenhuizen <jan@digicash.com>
*/

#ifndef BEZIER_HH
#define BEZIER_HH

#include "lily-proto.hh"
#include "real.hh"

/**
  Simple bezier curve
 */
class Bezier
{
public:
  Bezier (int steps_i);
  virtual ~Bezier ();

  /**
  Calculate bezier curve into Offset (x,y) array.
  */
  void calc (Offset control[4]);

  /**
  Return y that goes with x by interpolation.
  */
  Real y (Real x);

  int steps_i_;
  Offset* curve_;
};

/**
  Implement bow specific bezier curve
 */
class Bezier_bow : public Bezier
{
public:
  Bezier_bow (Paper_def* paper_l);

  /**
   Calculate bezier curve for bow from bow parameters.
   */
  void calc (Real dx, Real dy, Real h, Real d);
  Paper_def* paper_l_;
};


#endif // BEZIER_HH
