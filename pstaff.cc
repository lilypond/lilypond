#include "pstaff.hh"

PStaff::PStaff()
{
    stafsym = 0;
}

void
PStaff::add(Item *i )
{
    its.bottom().add(i);
    i->pstaff_ = this;
}
