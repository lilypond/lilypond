#ifndef DIMENSIONS_HH
#define DIMENSIONS_HH

#include "real.hh"
#include "string.hh"

const Real INCH_TO_PT=72.270;
const Real CM_TO_PT=INCH_TO_PT/2.54;
const Real MM_TO_PT=CM_TO_PT/10;
const Real PT_TO_PT =1.0;
const Real INCH_TO_BP = 72;
const Real BIGPOINT_TO_POINT = INCH_TO_PT/ INCH_TO_BP;
const Real CHAR_TO_PT =1.0;


#define PT  *PT_TO_PT
#define MM  *MM_TO_PT
#define CM  *CM_TO_PT
#define INCH *INCH_TO_PT
#define BIGPOINT *BIGPOINT_TO_POINT
#define CHAR *CHAR_TO_PT

String print_dimen (Real);

#endif // DIMENSIONS_HH

