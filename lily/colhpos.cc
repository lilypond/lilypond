#include "colhpos.hh"
#include "real.hh"
#include "debug.hh"
#include "const.hh"
#include "vector.hh"

Col_hpositions::Col_hpositions()
{
    energy = INFTY;
}

void
Col_hpositions::add( PCol*c)
{
    cols.push(c);
}

void
Col_hpositions::print() const
{
#ifndef NPRINT
    mtor << "energy : " << energy << '\n';
    mtor << "line of " << config.size() << " cols\n";
    Vector v(config);
    mtor << v;
#endif
}

void
Col_hpositions::OK()const
{
#ifndef NDEBUG
    assert(config.size() == cols.size());
#endif
}
