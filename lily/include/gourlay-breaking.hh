/*
  gourlay-breaking.hh -- declare Gourlay_breaking

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef GOURLAY_BREAKING_HH
#define GOURLAY_BREAKING_HH

#include "break-algorithm.hh"

/**
  A dynamic programming solution to breaking scores into lines
 */
struct Gourlay_breaking : public Break_algorithm
{
  Array<Column_x_positions> do_solve () const;
  Gourlay_breaking ();
  Real combine_demerits (Column_x_positions const&,Column_x_positions const&) const;
};
#endif // GOURLAY_BREAKING_HH
