#include "debug.hh"
#include "item.hh"

Item::Item()
{
    pcol_l_ = 0;
}

void
Item::print() const
{
#ifndef NPRINT
    mtor << "item " ;
    Staff_elem::print();
#endif

}

