#include "pstaff.hh"
#include "pscore.hh"
#include "symbol.hh"
#include "molecule.hh"
#include "pcol.hh"

void
Item::translate(Offset O)
{
    offset_ += O;
}

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
    Item * me = (Item*) this;
    output->translate(offset_);	// ugh?
    me ->offset_ = Offset(0,0);	// URRGGH!
    return output->TeXstring();
}

Interval
Item::width() const
{
    Interval i =output->extent().x ;
    return i+=offset_.x;
}

Interval
Item::height() const
{
    Interval i =output->extent().y;
    return i+=offset_.y;
}

Item::~Item()
{
    delete output;
}

Item::Item()
{
    pcol_ = 0;
    output = 0;
    pstaff_ = 0;
}
void
Item::print() const
{
    assert(output);
    output->print();
}

Paperdef*
Item::paper()  const
{
    assert(pstaff_);
    return pstaff_->pscore_->paper_;
}

