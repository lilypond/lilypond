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

    /****************/
    
    Real center() { return (min + max) /2;}
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
    /**
      PRE
      *this and h are comparable
      */
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
    String str() const;

    /// partial ordering
    static compare(const Interval&,Interval const&);
    /**
      inclusion ordering. Crash if not comparable.
      */
};
/**
  this represents the closed interval [min,max].
  No invariants
  */

Interval intersection(Interval, Interval const&);

#include "compare.hh"

instantiate_compare(Interval&, Interval::compare);


inline
Interval operator +(double a,Interval i )
{
    i += a;
    return i;
}

inline
Interval operator +(Interval i,double a ){
    return a+i;
}

#endif // INTERVAL_HH



