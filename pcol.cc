#include "pcol.hh"
#include "pstaff.hh"
#include "debug.hh"

void
Idealspacing::print() const
{
    #ifndef NPRINT
    mtor << "idealspacing {" ;
    mtor << "distance "<<space<< " strength " << hooke << "}\n";
    #endif
}

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
#ifndef NDEBUG
    assert(hooke >= 0 && left  && right);
#endif    
}

/****************************************************************/

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

void
PCol::print() const
{
    #ifndef NPRINT
    mtor << "PCol {";
    mtor << "# symbols: " << its.size() ;
    mtor << "breakable: " << breakable<<"\n";
    mtor << "extent: " << width().min << ", " << width().max << "\n";
    mtor << "}\n";
    #endif 
}

int
PCol::compare(const PCol &, const PCol &)
{
    assert(false);
    return 0 ;
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
    if (prebreak)
	delete prebreak;	// no recursion!
    if (postbreak)
	delete postbreak;	
}

void
PCol::add(const Item *i)
{
    its.bottom().add(i);
    used = true;
}

