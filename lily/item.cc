#include "debug.hh"
#include "item.hh"



Item::Item()
{
    pcol_l_ = 0;
}

IMPLEMENT_STATIC_NAME(Item);

void
Item::do_print() const
{
#ifndef NPRINT
    mtor << "(unknown)";
#endif
}

