#ifndef DIMENSIONS_HH
#define DIMENSIONS_HH

#include "real.hh"
class String;


const Real INCH_TO_PT=72.270;
const Real CM_TO_PT=(INCH_TO_PT/2.54);
const Real MM_TO_PT=(CM_TO_PT/10);
const Real PT_TO_PT =1.0;
const Real INCH_TO_BP = 72;
const Real BIGPOINT_TO_POINT = (INCH_TO_PT/ INCH_TO_BP);
const Real CHAR_TO_PT =1.0;
const Real PT_TO_MM = (1.0/MM_TO_PT);

#ifdef POINTS

#define PT  *PT_TO_PT
#define MM  *MM_TO_PT
#define CM  *CM_TO_PT
#define INCH *INCH_TO_PT
#define BIGPOINT *BIGPOINT_TO_POINT
#define CHAR *CHAR_TO_PT


#define INTERNAL_UNIT "pt"

#else	// mm

#define PT  *PT_TO_PT *PT_TO_MM
#define MM  *MM_TO_PT *PT_TO_MM
#define CM  *CM_TO_PT *PT_TO_MM
#define INCH *INCH_TO_PT *PT_TO_MM
#define BIGPOINT *BIGPOINT_TO_POINT *PT_TO_MM
#define CHAR *CHAR_TO_PT *PT_TO_MM
#define INTERNAL_UNIT "mm"

#endif

String print_dimen (Real);

#endif // DIMENSIONS_HH

