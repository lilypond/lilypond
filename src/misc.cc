#include "misc.hh"
#include "glob.hh"

#include <math.h>

Real
wholes(int dur, int dots)
{
    Real f = 1.0/Real(dur);
    Real delta = f;

    while (dots--) {
	delta /= 2.0;
	f += delta;
    }
    return f;    
}
int
intlog2(int d) {
    int i=0;
    while (!(d&1)) {
	d/= 2;
	i++;
    }
    assert(!(d/2));
    return i;
}

double
log_2(double x) {
    return log(x)  /log(2.0);
}

