#include <assert.h> 
#include "interval.hh"
#include <math.h>

const Real INFTY = HUGE;

void
Interval::set_empty() {
    min = INFTY;
    max = -INFTY;
}

Real
Interval::length() const {
    assert(max >= min);
    return max-min;
}

