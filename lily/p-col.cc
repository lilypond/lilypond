/*
  p-col.cc -- implement PCol

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "p-col.hh"
#include "p-score.hh"

#include "debug.hh"

Interval
PCol::width() const
{
    Interval w;

    for (iter_top(its,i); i.ok(); i++)
	w.unite(i->width());
    if (w.empty_b())
	w.unite(Interval(0,0));
    return w;
}

void
PCol::clean_breakable_items()
{
    if (!line_l_) {
	its.junk_links();
    }
    if (prebreak_p_) 
	prebreak_p_->clean_breakable_items();
    if (postbreak_p_) 
	postbreak_p_->clean_breakable_items();
}

int
PCol::rank_i() const
{
    return rank_i_;
}

void
PCol::set_rank(int i)
{
    rank_i_ = i;
    if (prebreak_p_)
	prebreak_p_->rank_i_ = i;
    if (postbreak_p_)
	postbreak_p_->rank_i_ = i;
}

void
PCol::print() const
{
#ifndef NPRINT
    mtor << "PCol {";

    mtor << "rank: " << rank_i_ << '\n';

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
PCol::compare(PCol const &c1, PCol const &c2)
{
    return c1.rank_i() - c2.rank_i();
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

    do_set_breakable();
    prebreak_p_->pscore_l_ = pscore_l_;
    postbreak_p_->pscore_l_ = pscore_l_;

    prebreak_p_->daddy_l_ = postbreak_p_->daddy_l_ = this;
}
void
PCol::do_set_breakable()
{
     prebreak_p_ = new PCol;
    postbreak_p_ = new PCol;
}
  

bool
PCol::breakpoint_b() const
{
    return !line_l_;
}

bool
PCol::breakable_b() const
{
    return prebreak_p_||postbreak_p_;
}

PCol::PCol()
{
    used_b_ = false;
    error_mark_b_ = false;
    daddy_l_ = 0;
    prebreak_p_=0;
    postbreak_p_=0;
    line_l_=0;
    hpos_f_ = -1.0;
    pscore_l_ = 0;
    rank_i_ = -1;
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
    return daddy_l_ || breakable_b() || its.size()|| used_b_;
}
