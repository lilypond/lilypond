#include "misc.hh"
#include "glob.hh"
#include "moment.hh"

#include <math.h>

Moment
wholes(int dur, int dots)
{
    if (!dur)
	return 0.0;

    // stupid Intel: doesn't crash if !dur
    Moment f = 1/Moment(dur);
    Moment delta = f;

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

