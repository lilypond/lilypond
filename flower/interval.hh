/*
  interval.hh -- part of flowerlib
  
  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INTERVAL_HH
#define INTERVAL_HH

#include <assert.h> 
#include "fproto.hh"
#include "real.hh"


/// a T interval
template<class T>
struct Interval_t {
    T left, right;

    /****************/
    
    T center() { return (left + right) /2;}
    void translate(T t) {
	left += t;
	right += t;
    }
    T& idx(int j) {
	if (j==-1)
	    return left;
	else if (j==1)
	    return right;
	else
	    assert(false);
	return left;		
    }
    T& operator[](int j) {
	return idx(j);
    }
    T operator[](int j) const {
	return ((Interval_t<T> *)this)->idx(j);
    }
    T &max() { return right;}
    T max()const { return right;}
    T min()const{ return left; }
    T &min(){ return left; }
    void unite(Interval_t<T> h);
    /**
      PRE
      *this and h are comparable
      */
    void intersect(Interval_t<T> h);

    T length() const;
    void set_empty() ;
    bool empty() const { return left > right; }
    Interval_t() {
	set_empty();
    }
    Interval_t(T m, T M) {
	left =m;
	right = M;
    }
    Interval_t<T> &operator += (T r) {
	left += r;
	right +=r;
	return *this;
    }
    String str() const;    
    bool elt_q(T r);
};
/**
  this represents the closed interval [left,right].
  No invariants. T must be a totally ordered ring
  */

/// partial ordering
template<class T>
int Interval__compare(const Interval_t<T>&,Interval_t<T> const&);
/**
  inclusion ordering. Crash if not comparable.
  */

/****************************************************************
  INLINE
 ****************************************************************/

#include "compare.hh"

template_instantiate_compare(Interval_t<T>&, Interval__compare, template<class T>);


template<class T>
inline Interval_t<T>
intersection(Interval_t<T> a, Interval_t<T> const&b)
{
    a.intersect(b);
    return a;
    
}


template<class T>
inline
Interval_t<T> operator +(T a,Interval_t<T> i )
{
    i += a;
    return i;
}

template<class T>
inline
Interval_t<T> operator +(Interval_t<T> i,T a ){
    return a+i;
}

typedef Interval_t<Real> Interval;


#define Interval__instantiate(T) template struct Interval_t<T>;\
  template  int Interval__compare(const Interval_t<T>&,Interval_t<T> const&)


#endif // INTERVAL_HH



