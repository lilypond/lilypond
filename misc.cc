#include "misc.hh"
#include "glob.hh"
#include "mtime.hh"
#include <math.h>

int intlog2(int d) {
    int i=0;
    while (!(d&1)) {
	d/= 2; i++;
    }
    assert(!(d/2));
    return i;
}

double log2(double x) {
    return log(x)  /log(2.0);
}


// golden ratio
 
const Real PHI = (1+sqrt(5))/2;
const double ENGRAVERS_SPACE = PHI;
const double WHOLE_SPACE = 10.0;

  
Real
duration_to_idealspace(Mtime d)
{
    // see  Roelofs, p. 57
    return WHOLE_SPACE * pow(ENGRAVERS_SPACE, log2(d));
}
