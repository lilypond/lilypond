#include "leastsquares.hh"

void
Least_squares::OK() const
{
  assert (input.size() > 1);
  Real dx = 0.0;
  for (int i=1; i < input.size(); i++)
	dx += abs (input[i-1].x() - input[i].x ());

  assert (dx);
}

void
Least_squares::minimise (Real &coef, Real &offset)
{
  OK();
  Real sx = 0.0;
  Real sy = 0.0;
  Real sqx =0.0;
  Real sxy = 0.0;

  for (int i=0; i < input.size();i++) 
    {
	Real x=input[i].x();
	Real y = input[i].y();
	sx += x;
	sy += y;
	sqx += sqr (x);
	sxy += x*y;
    }
  int N = input.size();
  

  coef = (N * sxy - sx*sy)/(N*sqx - sqr (sx));
  offset = (sy - coef * sx)/N;
	
}
