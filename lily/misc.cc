/*
  misc.cc -- implement various stuff

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <math.h>

#include "item.hh"
#include "misc.hh"
#include "moment.hh"

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

#if 1
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

#endif
