#include <limits.h>
#include "proto.hh"
#include "string.hh"
#include "moment.hh"
#include "real.hh"

#include "interval.tcc"

Interval__instantiate(Rational);
Interval__instantiate(int);

#ifdef AIX
const Real INFTY = 1e8;	// ARGh. AIX sucks
#else
const Real INFTY = HUGE_VAL;
#endif

Rational
Interval_t<Rational>::infinity()
{
    return INFTY;
}

int
Interval_t<int>::infinity()
{
    return INT_MAX;
}
