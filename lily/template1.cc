#include "proto.hh"
#include "idealspacing.hh"
#include "plist.hh"
#include "p-col.hh"
#include "item.hh"
#include "musical-request.hh"
#include "spanner.hh"
#include "scoreline.hh" 
#include "staffline.hh"

#include "pcursor.tcc"
#include "plist.tcc"


#define IPLC_instantiate(a) IPL_instantiate(a); PL_instantiate(const a)


IPLC_instantiate(Line_of_score);
IPLC_instantiate(Line_of_staff);
IPLC_instantiate(Item);
IPLC_instantiate(Spanner);
IPLC_instantiate(PStaff);
IPLC_instantiate(Idealspacing);
IPLC_instantiate(PCol);

