#include "line.hh"

#include "plist.hh"
#include "pcol.hh"
#include "item.hh"
#include "request.hh"
#include "command.hh"
#include "spanner.hh"

#include "list.cc"
#include "plist.cc"
#include "cursor.cc"

#define PLC_instantiate(a) PL_instantiate(a); PL_instantiate(const a)

PLC_instantiate(Line_of_score);
PLC_instantiate(Line_of_staff);
PLC_instantiate(Item);
PLC_instantiate(Spanner);
PLC_instantiate(PStaff);
PLC_instantiate(Idealspacing);
PLC_instantiate(PCol);

