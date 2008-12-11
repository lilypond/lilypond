/*
  least-squares.cc -- implement minimise_least_squares

  source file of the GNU LilyPond music typesetter

  (c) 1996--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "least-squares.hh"

#include "warn.hh"

void
minimise_least_squares (Real *coef, Real *offset,
			vector<Offset> const &input)
{
  Real sx = 0.0;
  Real sy = 0.0;
  Real sqx = 0.0;
  Real sxy = 0.0;

  for (vsize i = 0; i < input.size ();i++)
    {
      Real x = input[i][X_AXIS];
      Real y = input[i][Y_AXIS];
      sx += x;
      sy += y;
      sqx += sqr (x);
      sxy += x*y;
    }

  int count = input.size ();

  *coef = 0.0;
  *offset = 0.;

  Real den = (count * sqx - sqr (sx));
  if (!count || !den)
    {
      programming_error ("minimise_least_squares ():  Nothing to minimise\n"
			 "This means that vertical spacing is triggered\n"
			 "before line breaking\n");
      *coef = 0.0;
      *offset = count ? sy / count : 0.0;
    }
  else
    {
      *coef = (count * sxy - sx * sy) / den;
      *offset = (sy - (*coef) * sx) / count;
    }
}

