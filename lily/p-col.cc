#include "p-col.hh"
#include "p-score.hh"
#include "p-staff.hh"
#include "debug.hh"

Interval
PCol::width() const
{
    Interval w;

    for (iter_top(its,i); i.ok(); i++)
	w.unite(i->width());
    if (w.empty())
	w.unite(Interval(0,0));
    return w;
}

int
PCol::rank() const
{
    if(!pscore_l_)
	return -1;
    PCursor<PCol*> me=pscore_l_->find_col( (PCol*)this);
    if (!me.ok())
	return -1;
    PCursor<PCol*> bot(pscore_l_->cols.top());
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
    if (breakable_b()){
	mtor << "\npre,post: ";
	prebreak_p_->print();
	postbreak_p_->print();
    } else if (daddy_l_) {
	mtor<<'\n' << ((this == daddy_l_->prebreak_p_) ?
		       "prebreak" : "postbreak");
	mtor << '\n';
    }
    mtor << "extent: " << width().str() << "\n";
    mtor << "}\n";
#endif 
}

int
PCol::compare(const PCol &c1, const PCol &c2)
{
    PScore*ps_l = c1.pscore_l_;
    PCursor<PCol*> ac(ps_l->find_col(&c1));
    PCursor<PCol*> bc(ps_l->find_col(&c2));
    assert(ac.ok() && bc.ok());
    return ac - bc;
}

void
PCol::OK() const
{
#ifndef NDEBUG
    if (prebreak_p_ || postbreak_p_ ) {
	assert(prebreak_p_&&postbreak_p_);
	assert(prebreak_p_->daddy_l_ == this);
	assert(postbreak_p_->daddy_l_ == this);
    }
#endif
}

void
PCol::set_breakable()
{
    if (breakable_b())
	return;

    prebreak_p_ = new PCol(this);
    postbreak_p_ = new PCol(this);
    prebreak_p_->pscore_l_ = pscore_l_;
    postbreak_p_->pscore_l_ = pscore_l_;
}

bool
PCol::breakable_b() const
{
    return prebreak_p_||postbreak_p_;
}

PCol::PCol(PCol *parent)
{
    daddy_l_ = parent;
    prebreak_p_=0;
    postbreak_p_=0;
    line_l_=0;
    hpos = -1.0;
    pscore_l_ = 0;
}

PCol::~PCol()
{
    delete prebreak_p_;
    delete postbreak_p_;	
}

void
PCol::add( Item *i)
{
    its.bottom().add(i);
    i->pcol_l_ = this; 
}

bool
PCol::used_b()const
{
    return breakable_b() || its.size();
}
