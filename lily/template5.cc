/*
  template5.cc -- instantiate Intervals

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <limits.h>
#include "proto.hh"
#include "string.hh"
#include "moment.hh"
#include "real.hh"

#include "interval.tcc"

Interval__instantiate(Rational);
Interval__instantiate(int);

const Real INFTY_f = (Real)INT_MAX;
#if defined AIX || defined _WIN32
const Real INFTY = 1e8;	// ARGh. AIX sucks -- so does doze
#else
const Real INFTY = HUGE_VAL;
#endif

Rational
Interval_t<Rational>::infinity()
{
/*
  windhoze-suck-suck-suck-suck-suck-thank-you-cygnus

  I get tired of all these incompatibilities. We'll just assume:

  that 2^31 is *Big*

 */

    return Rational(INT_MAX);
}

int
Interval_t<int>::infinity()
{
    return INT_MAX;
}
