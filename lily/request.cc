#include "request.hh"
#include "debug.hh"

IMPLEMENT_STATIC_NAME(Request);
IMPLEMENT_IS_TYPE_B1(Request,Music);

void
Request::do_print() const
{
}

MInterval
Request::time_int() const
{
    return MInterval(0, duration());
}

void
Request::print() const
{
#ifndef NPRINT
    mtor << name() << " {";
    do_print();
    mtor << "}\n";
#endif
}
  
