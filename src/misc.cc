#include <math.h>

#include "item.hh"
#include "misc.hh"
#include "glob.hh"
#include "moment.hh"


Moment
wholes(int dur, int dots)
{
    if (!dur)
	return 0;

    Moment f = Rational(1)/Moment(dur);
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

Interval
itemlist_width(const Array<Item*> &its)
{
    Interval iv ;
    iv.set_empty();
     
    for (int j =0; j < its.size(); j++){
	iv.unite (its[j]->width());

    }
    return iv;
}

