#ifndef REAL_HH
#define REAL_HH


#include <builtin.h>
#include <minmax.h>
#include <std/cmath.h>

typedef double Real;

inline Real
distance(Real x,Real y)
{
    return abs(x-y);
}
#endif
