#include "boxes.hh"
#include "const.hh"
#include "vray.hh"


Box::Box()
{        
}

Box::Box(Interval ix, Interval iy)
{
    x = ix;
    y = iy;
}

