/*
  p-score.cc -- implement PScore

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "super-elem.hh"
#include "idealspacing.hh"
#include "debug.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "paper-def.hh"
#include "scoreline.hh"
#include "p-score.hh"
#include "tex-stream.hh"
#include "break.hh"
#include "p-col.hh"

void
PScore::typeset_element(Score_elem * elem_p)
{
    elem_p_list_.bottom().add(elem_p);
    elem_p->pscore_l_ = this;
    elem_p->add_processing();
}

void
PScore::typeset_item(Item *i, PCol *c, int breakstat)
{
    assert(c && i);

    if (breakstat == 0) {
	typeset_item(i, c->prebreak_p_);
	return;
    }

    if (breakstat == 2) {
	typeset_item(i, c->postbreak_p_);
	return;
    }

    c->add(i);
    typeset_element(i);
}

void
PScore::typeset_broken_spanner(Spanner*span_p)
{
    span_p->left_col_l_->starters.bottom().add (span_p);
    span_p->right_col_l_->stoppers.bottom().add(span_p);
    assert(span_p->left_col_l_->line_l_ == span_p->right_col_l_->line_l_);

    typeset_element(span_p);
}


void
PScore::typeset_unbroken_spanner(Spanner*span_p)
{
    spanners.bottom().add(span_p);
    span_p->pscore_l_=this;
    // do not init start/stop fields. These are for broken spans only.
    span_p->add_processing();
}

Idealspacing*
PScore::get_spacing(PCol*l, PCol*r)
{
    assert(l!=r);

    Idealspacing*i_p =new Idealspacing(l,r);
    suz.bottom().add(i_p);

    return i_p;
}


void
PScore::clean_cols()
{
    int rank_i = 0;
    for (iter_top(cols,c); c.ok(); )
	if (!c->used_b()) {
	    delete c.remove_p();
	} else {
	    c->set_rank(rank_i++);
	    c++;
	}
}

void
PScore::do_connect(PCol *c1, PCol *c2, Real d, Real h)
{
    if (!c1 || !c2 )
	return;
    Idealspacing*s_l=get_spacing(c1,c2);

    
    s_l->hooke = h;
    s_l->space =d;
}

void
PScore::connect(PCol* c1, PCol *c2, Real d, Real h)
{
    do_connect(c1,c2,d,h);
    do_connect(c1->postbreak_p_, c2,d,h);
    do_connect(c1, c2->prebreak_p_,d,h);
    do_connect(c1->postbreak_p_, c2->prebreak_p_,d,h);
}

void
PScore::add(PCol *p)
{
    p->pscore_l_ = this;
    if (p->breakable_b()){
	p->prebreak_p_->pscore_l_ = this;
	p->postbreak_p_->pscore_l_ = this;
    }
    cols.bottom().add(p);
}

PScore::PScore(Paper_def*p)
{
    paper_l_ = p;
    super_elem_l_   = new Super_elem;
    typeset_element(super_elem_l_);
}

void
PScore::output(Tex_stream &ts)
{
    ts << "\n "<<  paper_l_->lookup_l()->texsetting << "%(Tex id)\n";
    ts<< super_elem_l_->TeX_string();
    ts << "\n\\EndLilyPondOutput";
}


PScore::~PScore()
{
    super_elem_l_->unlink_all();
}

void
PScore::OK()const
{
#ifndef NDEBUG
    for (iter_top(cols,cc); cc.ok(); cc++)
	cc->OK();
    for (iter_top(suz,i); i.ok(); i++)
	i->OK();
#endif
}

void
PScore::print() const
{    
#ifndef NPRINT
    mtor << "PScore { ";
    paper_l_->print();
    mtor << "\n elements: ";
    for (iter_top(elem_p_list_,cc); cc.ok(); cc++)	
	cc->print();
    mtor << "\ncolumns: ";
     for (iter_top(cols,cc); cc.ok(); cc++)
	cc->print();
    
    mtor << "\nideals: ";
    for (iter_top(suz,i); i.ok(); i++)
	i->print();
    mtor << "}\n";
#endif 
}

void
PScore::preprocess()
{
    super_elem_l_->pre_processing();
}

void
PScore::postprocess()
{
    super_elem_l_->post_processing();
    super_elem_l_->molecule_processing();
}

PCursor<PCol *>
PScore::find_col(PCol const *c)const
{
    PCol const *what = c;
    if (what->daddy_l_ )
	what = what->daddy_l_;
    
    return cols.find((PCol*)what);
}


void
PScore::set_breaking(Array<Col_hpositions> const &breaking)
{
    super_elem_l_->line_of_score_l_->set_breaking( breaking);
    super_elem_l_->break_processing();


    for (iter_top(spanners,i); i.ok(); ) {
	Spanner *span_p = i.remove_p();
	if (span_p->broken_b()) {
	    span_p->unlink();
	    delete span_p;
	}else{
	    typeset_broken_spanner(span_p);
	}
    }
}

void
PScore::calc_breaking()
{
    Word_wrap w(*this);
    set_breaking(w.solve());
}

void
PScore::process()
{
    clean_cols();
    print();
    *mlog << "Preprocessing elements... " <<flush;
    preprocess();
    *mlog << "\nCalculating column positions ... " <<flush;
    calc_breaking();
    *mlog << "\nPostprocessing elements..." << endl;
    postprocess();
}
/** Get all breakable columns between l and r, (not counting l and r).  */
Link_array<PCol>
PScore::breakable_col_range(PCol*l,PCol*r)const
{
    Link_array<PCol> ret;

    PCursor<PCol*> start(l ? find_col(l)+1 : cols.top() );
    PCursor<PCol*> stop(r ? find_col(r) : cols.bottom());
  
    while ( start < stop ) {
	if (start->breakable_b())
	    ret.push(start);
	start++;
    }

    return ret;
}
Link_array<PCol>
PScore::col_range(PCol*l,PCol*r)const
{
    Link_array<PCol> ret;
    
    PCursor<PCol*> start(l ? find_col(l)+1 : cols.top() );
    PCursor<PCol*> stop(r ? find_col(r) : cols.bottom());
    ret.push(l);
    while ( start < stop )
	ret.push(start++);
    ret.push(r);
    return ret;
}

Link_array<PCol>
PScore::broken_col_range(PCol*l,PCol*r)const
{
    Link_array<PCol> ret;

    PCursor<PCol*> start(l ? find_col(l)+1 : cols.top() );
    PCursor<PCol*> stop(r ? find_col(r) : cols.bottom());
  
    while ( start < stop ) {
	if (start->breakable_b() && !start->line_l_ )
	    ret.push(start);
	start++;
    }

    return ret;
}
