#ifndef DIMENSIONS_HH
#define DIMENSIONS_HH

#include "real.hh"
#include "std-string.hh"

const Real INCH_TO_PT = 72.270;
const Real CM_TO_PT = (INCH_TO_PT / 2.54);
const Real MM_TO_PT = (CM_TO_PT / 10);
const Real PT_TO_PT = 1.0;
const Real INCH_TO_BP = 72;
const Real BIGPOINT_TO_POINT = (INCH_TO_PT / INCH_TO_BP);
const Real CHAR_TO_PT = 1.0;
const Real PT_TO_MM = (1.0 / MM_TO_PT);

#ifdef POINTS

#define INTERNAL_UNIT "pt"

#define PT *(PT_TO_PT)
#define MM *(MM_TO_PT)
#define CM *(CM_TO_PT)
#define INCH *(INCH_TO_PT)
#define BIGPOINT *(BIGPOINT_TO_POINT)
#define CHAR *(CHAR_TO_PT)

#else // mm

#define INTERNAL_UNIT "mm"

#define PT *(PT_TO_PT *PT_TO_MM)
#define MM *(MM_TO_PT *PT_TO_MM)
#define CM *(CM_TO_PT *PT_TO_MM)
#define INCH *(INCH_TO_PT *PT_TO_MM)
#define BIGPOINT *(BIGPOINT_TO_POINT *PT_TO_MM)
#define CHAR *(CHAR_TO_PT *PT_TO_MM)

#endif

std::string print_dimen (Real);
const Real point_constant = 1 PT;
const Real inch_constant = 1 INCH;
const Real cm_constant = 1 CM;
const Real mm_constant = 1 MM;
const Real bigpoint_constant = 1 BIGPOINT;

#endif /* DIMENSIONS_HH */

