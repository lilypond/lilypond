#include "leastsquares.hh"
#include "warn.hh"

void
Least_squares::OK() const
{
  assert (input.size() > 1);
  Real dx = 0.0;
  for (int i=1; i < input.size(); i++)
    dx += abs (input[i-1][X_AXIS] - input[i][X_AXIS]);

  assert (dx);
}

void
Least_squares::minimise (Real &coef, Real &offset)
{
  Real sx = 0.0;
  Real sy = 0.0;
  Real sqx =0.0;
  Real sxy = 0.0;

  for (int i=0; i < input.size();i++) 
    {
      Real x=input[i][X_AXIS];
      Real y = input[i][Y_AXIS];
      sx += x;
      sy += y;
      sqx += sqr (x);
      sxy += x*y;
    }
  int N = input.size();

  coef =0.0;
  offset =0.;
  
  Real den = (N*sqx - sqr (sx));
  if (!N || !den)
    programming_error ("Least_squares::minimise():  Nothing to minimise");
  coef = (N * sxy - sx*sy)/den;
  offset = (sy - coef * sx)/N;
}
