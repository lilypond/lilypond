#include "misc.hh"
#include "glob.hh"

#include <math.h>

int intlog2(int d) {
    int i=0;
    while (!(d&1)) {
	d/= 2; i++;
    }
    assert(!(d/2));
    return i;
}

