#include "pstaff.hh"
#include "molecule.hh"

PStaff::~PStaff()
{
    delete stafsym_p_;
}

PStaff::PStaff(PScore*ps)
{
    pscore_l_=ps;
    stafsym_p_ = 0;
}

void
PStaff::add(Item *i )
{
    its.bottom().add(i);
    i->pstaff_l_ = this;
}
