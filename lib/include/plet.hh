/*
  plet.hh -- declare Plet

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef PLET_HH
#define PLET_HH
#include "moment.hh"

/** 
  The type and replacement value of a  plet (triplet, quintuplet.) Conceptually the same as a rational, but 4/6 != 2/3.
  
  (plet)
 */
struct Plet {
  Plet ();
  Moment mom () const;
  bool unit_b () const;
  int iso_i_;  // 2/3; 2 is not duration, maar of count!
  int type_i_; 
};

#endif // PLET_HH
