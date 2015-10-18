#include "real.hh"

#include <cmath>
using namespace std;

#ifdef INFINITY
const Real infinity_f = INFINITY;
#else
const Real infinity_f = HUGE_VAL;
#endif

