#ifndef MISC_HH
#define MISC_HH

#include "real.hh"
#include "varray.hh"
#include "interval.hh"

double log_2(double x) ;
int intlog2(int d);
#if ! defined(_ABS_)
inline int
abs (int i){
    return (i < 0)?-i:i;
}
#endif
inline int
sign (int i) {
    if (i<0) return -1;
    else if (i) return 1;
    else return 0;
}

#ifndef STANDALONE
#include "lily-proto.hh"
Interval itemlist_width (const Array<Item*> &its);
#endif

int get_lower_bound (Array<Real> const& positions, Real x);
Slice get_bounds_slice (Array<Real> const& positions, Real x);
Interval get_bounds_iv (Array<Real> const& positions, Real x);
Interval quantise_iv (Array<Real> const& positions, Real period, Real x);

#endif

