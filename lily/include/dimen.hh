#ifndef DIMEN_HH
#define DIMEN_HH

#include "real.hh"
#include "string.hh"

const Real INCH_TO_PT=72.270;
const Real CM_TO_PT=INCH_TO_PT/2.54;
const Real MM_TO_PT=CM_TO_PT/10;
const Real PT_TO_PT =1.0;

#define PT  *PT_TO_PT
#define MM  *MM_TO_PT
#define CM  *CM_TO_PT
#define INCH *INCH_TO_PT

Real parse_dimen (String);
String print_dimen (Real);
Real convert_dimen (Real, String);
#endif

