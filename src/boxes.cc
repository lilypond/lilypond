#include "boxes.hh"
#include "const.hh"
#include "varray.hh"


Box::Box()
{        
}

Box::Box(Interval ix, Interval iy)
{
    x = ix;
    y = iy;
}

