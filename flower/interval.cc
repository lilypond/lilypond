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
