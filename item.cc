#include "line.hh"
#include "symbol.hh"
#include "molecule.hh"
#include "pcol.hh"

String
Spanner::TeXstring() const
{
    assert(right->line);
    Real w = left->hpos - right->hpos;
    return strets->eval(w).tex;
}

Spanner *
Spanner::broken_at(const PCol *c1, const PCol *c2) const
{
    Spanner *sp = new Spanner(*this);
    sp->left = c1;
    sp->right = c2;
    return sp;
}

Spanner::Spanner()
{
    pstaff_=0;
    strets=0;
    left = right = 0;
}

/****************************************************************/
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

/****************************************************************/

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
