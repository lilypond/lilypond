#include "pstaff.hh"

PStaff::PStaff(PScore*ps)
{
    pscore_=ps;
    stafsym = 0;
}

void
PStaff::add(Item *i )
{
    its.bottom().add(i);
    i->pstaff_ = this;
}
