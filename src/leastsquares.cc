#include "leastsquares.hh"

void
Least_squares::minimise(Real &coef, Real &offset)
{
    Real sx = 0.0;
    Real sy = 0.0;
    Real sqx =0.0;
    Real sxy = 0.0;

    for (int i=0; i < input.sz();i++) {
	Real x=input[i].x;
	Real y = input[i].y;
	sx += x;
	sy += y;
	sqx += sqr(x);
	sxy += x*y;
    }
    int N = input.sz();
    

    coef = (N * sxy - sx*sy )/(N*sqx - sqr(sx));
    offset = (sy - coef * sx)/N;
	
}
