#include "pcol.hh"
#include "pscore.hh"
#include "pstaff.hh"
#include "debug.hh"

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
int
PCol::rank() const
{
    if(!pscore_)
	return -1;
    PCursor<PCol*> me=pscore_->find_col( (PCol*)this);
    if (!me.ok())
	return -1;
    PCursor<PCol*> bot(pscore_->cols.top());
    return me - bot;
}

void
PCol::print() const
{
#ifndef NPRINT
    mtor << "PCol {";

    if (rank() >= 0)
	mtor << "rank: " << rank() << '\n';

    mtor << "# symbols: " << its.size() ;
    if (breakable()){
	mtor << "\npre,post: ";
	prebreak->print();
	postbreak->print();
    } else if (daddy) {
	mtor<<'\n' << ((this == daddy->prebreak) ? "prebreak" : "postbreak");
    }
    mtor << "extent: " << width().min << ", " << width().max << "\n";
    mtor << "}\n";
#endif 
}

int
PCol::compare(const PCol &c1, const PCol &c2)
{
    return c1.pscore_->compare_pcols((PCol*)&c1,(PCol*)&c2);
}

void
PCol::OK () const
{
    if (prebreak || postbreak ) {
	assert(prebreak&&postbreak);
	assert(prebreak->daddy == this);
	assert(postbreak->daddy == this);
    }    
}

void
PCol::set_breakable()
{
    if (breakable())
	return;

    prebreak = new PCol(this);
    postbreak = new PCol(this);
    prebreak->pscore_ = pscore_;
    postbreak->pscore_ = pscore_;
    
 
}

bool
PCol::breakable() const
{
    return prebreak||postbreak;
}

PCol::PCol(PCol *parent)
{
    daddy = parent;
    prebreak=0;
    postbreak=0;
    line=0;
 
    pscore_ = 0;
}

PCol::~PCol()
{
    delete prebreak;
    delete postbreak;	
}

void
PCol::add( Item *i)
{
    its.bottom().add(i);
    i->pcol_ = this;
 
}

bool
PCol::used()const
{
    return breakable() || its.size();
}
