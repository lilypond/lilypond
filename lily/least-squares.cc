/*   
  least-squares.cc --  implement minimise_least_squares
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1996--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "least-squares.hh"
#include "warn.hh"


void
minimise_least_squares (Real * coef, Real * offset,
			Array<Offset> input)
{
  Real sx = 0.0;
  Real sy = 0.0;
  Real sqx =0.0;
  Real sxy = 0.0;

  for (int i=0; i < input.size ();i++) 
    {
      Real x=input[i][X_AXIS];
      Real y = input[i][Y_AXIS];
      sx += x;
      sy += y;
      sqx += sqr (x);
      sxy += x*y;
    }
  int N = input.size ();

  *coef =0.0;
  *offset =0.;
  
  Real den = (N*sqx - sqr (sx));
  if (!N || !den)
    {
      programming_error ("minimise_least_squares ():  Nothing to minimise");
      *coef = 0.0;
      *offset = N ? sy/N : 0.0;
    }
  else
    {
      *coef = (N * sxy - sx*sy)/den;
      *offset = (sy - (*coef) * sx)/N;
    }
}

