#include "boxes.hh"
#include "const.hh"
#include "vray.hh"

Box::Box(svec<Real> &s)
{
    assert(s.sz() == 4);
    x.min = s[0];
    x.max = s[1];
    y.min = s[2];
    y.max = s[3];
}

Box::Box()
{        
}

Box::Box(Interval ix, Interval iy)
{
    x = ix;
    y = iy;
}

