/*
  interval.hh -- part of flowerlib
  
  (c) 1996 Han-Wen Nienhuys
*/

#ifndef INTERVAL_HH
#define INTERVAL_HH

#include <assert.h> 
#include "flower-proto.hh"
#include "real.hh"
#include "drul-array.hh"

/** a T interval.  this represents the closed interval [left,right].
  No invariants. T must be a totally ordered ring (with division, anyway ..)
  At instantiation, the function infinity () has to be defined explicitely.
  
  */
template<class T>
struct Interval_t : public Drul_array<T> {

  /* ************** */
    
  static T infinity () ;
  static String T_to_str (T arg);
  T center () {
    assert (!empty_b ());
    return (elem (LEFT) + elem (RIGHT)) / T (2);
  }
  void translate (T t)
    {
      elem (LEFT) += t;
      elem (RIGHT) += t;
    }
  
  /**
    PRE
    *this and h are comparable
    */
  void unite (Interval_t<T> h);
  void intersect (Interval_t<T> h);

  T length () const;
  T delta () const;
  void set_empty () ;
  bool empty_b () const { return elem (LEFT) > elem (RIGHT); }
  bool contains_b (Interval_t<T> const&) const;
  Interval_t () {
    set_empty ();
  }
  Interval_t (T m, T M) : Drul_array<T> (m,M)
    {
    }
  Interval_t<T> &operator -= (T r) {
    *this += -r;
    return *this;
  }

  Interval_t<T> &operator += (T r) {
    elem (LEFT) += r;
    elem (RIGHT) +=r;
    return *this;
  }
  Interval_t<T> &operator *= (T r) {
    if (!empty_b ())
      {
	elem (LEFT) *= r;
	elem (RIGHT) *= r;
	if (r < T (0))
	  swap();

      }
    return *this;
  }

  Real linear_combination (Real x) const {
    return ((1.0 - x) * Real (elem (LEFT))  + (x + 1.0) * Real (elem (RIGHT))) * 0.5;
  }
  String str () const;    

  bool elem_b (T r);
  void negate () {
    T r = -elem (LEFT);
    T l = -elem (RIGHT);
    elem (LEFT) = l;
    elem (RIGHT) =r;
  }
  
  void swap ()
  {
    T t = elem (LEFT);
    elem (LEFT) = elem (RIGHT);
    elem (RIGHT) = t;
  }
};


/**
  inclusion ordering. Crash if not  comparable.
  */
template<class T>
int Interval__compare (const Interval_t<T>&,Interval_t<T> const&);

/**
   Inclusion ordering.  return -2 if not comparable
 */
template<class T>
int
_Interval__compare (const Interval_t<T>&a,Interval_t<T> const&b);


/*
  INLINE
 */

#include "compare.hh"

TEMPLATE_INSTANTIATE_COMPARE (Interval_t<T>&, Interval__compare, template<class T>);


template<class T>
inline Interval_t<T>
intersection (Interval_t<T> a, Interval_t<T> const&b)
{
  a.intersect (b);
  return a;
    
}

template<class T>
inline
Interval_t<T> operator + (T a,Interval_t<T> i)
{
  i += a;
  return i;
}

template<class T>
inline
Interval_t<T> operator - (Interval_t<T> i, T a)
{
  i += -a;
  return i;
}

template<class T>
inline
Interval_t<T> operator - (T a,Interval_t<T> i)
{
  i.negate ();
  i += a;
  return i;
}

template<class T>
inline
Interval_t<T> operator + (Interval_t<T> i,T a){
  return a+i;
}

template<class T>
inline
Interval_t<T> operator * (T a,Interval_t<T> i)
{
  i *= a;
  return i;
}

template<class T>
inline
Interval_t<T> operator * (Interval_t<T> i,T a){
  return a*i;
}

// again? see flower-proto.hh
typedef Interval_t<Real> Interval;
typedef Interval_t<int> Slice;	// weird name


#endif // INTERVAL_HH

