#include "p-staff.hh"
#include "molecule.hh"

PStaff::PStaff(PScore*ps)
{
    pscore_l_=ps;
}

void
PStaff::add(Item *i)
{
    its.bottom().add(i);
    i->pstaff_l_ = this;
}
