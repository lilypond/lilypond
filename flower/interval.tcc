#include <assert.h> 
#include <math.h>
#include "interval.hh"
#include "string.hh"



template<class T>
int
Interval__compare(const Interval_t<T>&a,Interval_t<T> const&b)
{
    if (a.left == b.left && a.right == b.right)
	return 0;
    
    if (a.left <= b.left && a.right >= b.right)
	return 1;

    if (a.left >= b.left && a.right <= b.right)
	return -1;

    assert(false);		// not comparable

    return 0;
}

const Real INFTY = HUGE;

template<class T>
void
Interval_t<T>::set_empty() {
    left = INFTY;
    right = -INFTY;
}

template<class T>
T
Interval_t<T>::length() const {
    assert(right >= left);
    return right-left;
}

template<class T>
void
Interval_t<T>::unite(Interval_t<T> h)
{
    if (h.left<left)
	left = h.left;
    if (h.right>right)
	right = h.right;
}

/**
  smallest Interval which includes *this and #h#
 */

template<class T>
void
Interval_t<T>::intersect(Interval_t<T> h)
{
#if defined (__GNUG__) && ! defined (__STRICT_ANSI__)
    left = h.left >? left;
    right = h.right <?right;
#else
    left = max(h.left, left);
    right = min(h.right, right);
#endif
}

template<class T>
Interval_t<T>
intersect(Interval_t<T> x, Interval_t<T> const &y)
{
    x.intersect(y);
    return x;
}

template<class T>
String
Interval_t<T>::str() const
{
    if (empty())
	return "[empty]";
    String s("[");
 
    return s + left + "," + right +"]";
}

template<class T>
bool
Interval_t<T>::elt_q(T r)
{
    return r >= left && r <= right;
}
