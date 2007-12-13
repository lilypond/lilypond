#include "real.hh"

#include <cmath>
using namespace std;

const Real infinity_f =
#ifdef INFINITY
  INFINITY
#else
  HUGE_VAL  
#endif
  ;
  
