#include "boxes.hh"
#include "varray.hh"


Box::Box()
{        
}

Box::Box(Interval ix, Interval iy)
{
    x = ix;
    y = iy;
}

