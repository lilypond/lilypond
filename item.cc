#include "line.hh"
#include "symbol.hh"
#include "molecule.hh"
#include "pcol.hh"

void
Item::postprocess()
{
    // default: do nothing
}


void
Item::preprocess()
{
    // default: do nothing
}

String
Item::TeXstring() const
{
    return output->TeXstring();
}

Interval
Item::width() const
{
    return output->extent().x;
}

Interval
Item::height() const
{
    return output->extent().y;
}

Item::~Item()
{
    delete output;
}

Item::Item()
{
    col = 0;
    output = 0;
    pstaff_ = 0;
}
void
Item::print() const
{
    output->print();
}
