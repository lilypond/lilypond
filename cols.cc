#include "cols.hh"
#include "pstaff.hh"

Idealspacing::Idealspacing(const PCol * l,const PCol * r)
{
    space = 0.0;
    hooke = 0.0;
    left = l;
    right = r;
}
void
Idealspacing::OK() const
{
    assert(hooke >= 0 && left  && right);
}

Interval
PCol::width() const
{
    Interval w;

    for (PCursor<const Item *> ic(its); ic.ok(); ic++)
	w.unite(ic->width());
    if (w.empty())
	w.unite(Interval(0,0));
    return w;
}
/****************************************************************/

int
PCol::compare(const PCol &c1, const PCol &c2)
{
    assert(false);
}

void
PCol::OK () const
{
    if (prebreak || postbreak ) {
	assert(breakable);
    }
}

void
PCol::set_breakable()
{
    if (breakable)
	return;

    prebreak = new PCol(this);
    postbreak = new PCol(this);
    breakable = true;
    used = true;
}

PCol::PCol(PCol *parent) {
    daddy = parent;
    prebreak=0;
    postbreak=0;
    breakable=false;
    line=0;
    used  = false;
}

PCol::~PCol()
{
    delete prebreak;
    delete postbreak;	
}

void
PCol::add(const Item *i)
{
    its.bottom().add(i);
    used = true;
}

