#include "line.hh"

#include "list.hh"
#include "cols.hh"
#include "item.hh"
#include "request.hh"
#include "score.hh"
#include "command.hh"
#include "staff.hh"

#include "list.cc"
#include "cursor.cc"


PL_instantiate(Line_of_staff);
PL_instantiate(Item);
PL_instantiate(Line_of_score);
PL_instantiate(Request);
PL_instantiate(Spanner);
PL_instantiate(PStaff);
PL_instantiate(Idealspacing);
PL_instantiate(PCol);
PL_instantiate(Command);
PL_instantiate(Score_column);


