/*
  interval.hh -- part of flowerlib
  
  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INTERVAL_HH
#define INTERVAL_HH

#include <assert.h> 
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
    void unite(Interval h) {
	if (h.min<min)
	    min = h.min;
	if (h.max>max)
	    max = h.max;
    }
    Real length() const;
    void set_empty() ;
    bool empty() { return min > max; }
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
};


#endif // INTERVAL_HH


