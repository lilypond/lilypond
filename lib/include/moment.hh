/*
  (c) 1996--98 Han-Wen Nienhuys
*/

#ifndef MOMENT_HH
#define MOMENT_HH

#include <limits.h>
#include "rational.hh"

class String;
typedef Rational Moment;


/**
  A really big time-moment.

  Windhoze-suck-suck-suck-suck-suck-thank-you-cygnus

  I get tired of all these incompatibilities.  Let's just assume that
  INT_MAX is really, really, really big.

  Can't we name this Saint_jut_mom (Sintjuttemis ?)  */
  

const Moment infinity_mom = INT_MAX;

#endif // 

