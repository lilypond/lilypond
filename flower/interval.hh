/*
  interval.hh -- part of flowerlib
  
  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INTERVAL_HH
#define INTERVAL_HH

#include <assert.h> 
#include "fproto.hh"
#include "real.hh"


/// a Real interval
struct Interval {
    Real min, max;

    void translate(Real t) {
	min += t;
	max += t;
    }
    Real operator[](int j) {
	if (j==-1)
	    return min;
	else if (j==1)
	    return max;
	else
	    assert(false);
	    return 0.0;
		
    }
    void unite(Interval h) ;
    void intersect(Interval h);

    Real length() const;
    void set_empty() ;
    bool empty() const { return min > max; }
    Interval() {
	set_empty();
    }
    Interval(Real m, Real M) {
	min =m;
	max = M;
    }
    Interval &operator += (Real r) {
	min += r;
	max +=r;
	return *this;
    }
    bool elt_q(Real r);
    operator String() const;

    /// partial ordering
    static compare(const Interval&,Interval const&);
};
/**
  this represents the closed interval [min,max]
  */

Interval intersection(Interval, Interval const&);

#include "compare.hh"

instantiate_compare(Interval&, Interval::compare);

#endif // INTERVAL_HH



