#include <assert.h> 
#include <math.h>
#include "interval.hh"
#include "string.hh"


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
void
Interval::unite(Interval h)
{
    if (h.min<min)
	min = h.min;
    if (h.max>max)
	max = h.max;
}
void
Interval::intersect(Interval h)
{
    min = MAX(h.min, min);
    max = MIN(h.max, max);
}
Interval
intersection(Interval a, Interval const&b)
{
    a.intersect(b);
    return a;
    
}
int
Interval::compare(const Interval&a,Interval const&b)
{
    if (a.min == b.min && a.max == b.max)
	return 0;
    
    if (a.min <= b.min && a.max >= b.max)
	return 1;

    if (a.min >= b.min && a.max <= b.max)
	return -1;

    assert(false);		// not comparable

    return 0;
}

Interval
intersect(Interval x, Interval const &y)
{
    x.intersect(y);
    return x;
}
    

Interval::operator String() const
{
    if (empty())
	return "[empty]";
    String s("[");
 
    return s + min + "," + max +"]";
}
bool
Interval::elt_q(Real r)
{
    return r >= min && r <= max;
}
