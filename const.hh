/*
    global constants
    */
#ifndef CONST_HH
#define CONST_HH
#include <math.h>
#include "real.hh"

const Real CM_TO_PT=72/2.54;
const Real VERT_TO_PT=CM_TO_PT;	// tex output
const Real HOR_TO_PT=CM_TO_PT;	// tex output

const Real EPS=1e-7;		// qlpsolve.hh
const int MAXITER=100;		// qlpsolve.hh
const Real INFTY=HUGE;
#endif
