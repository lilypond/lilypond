/*
  p-score.cc -- implement PScore

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "super-elem.hh"
#include "debug.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "paper-def.hh"
#include "scoreline.hh"
#include "p-score.hh"
#include "tex-stream.hh"
#include "word-wrap.hh"
#include "p-col.hh"

PScore::PScore(Paper_def*p)
{
    paper_l_ = p;
    super_elem_l_   = new Super_elem;
    typeset_element(super_elem_l_);
}

PScore::~PScore()
{
    super_elem_l_->unlink_all();
}

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

    if (breakstat == -1) {
	typeset_item(i, c->prebreak_p_);
	return;
    }

    if (breakstat == 1) {
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
    span_p_list_.bottom().add(span_p);
    span_p->pscore_l_=this;

    if (span_p->left_col_l_) 
	span_p->left_col_l_->used_b_ = true;
    if ( span_p->right_col_l_)
	span_p->right_col_l_->used_b_ = true;
    
    // do not init start/stop fields. These are for broken spans only.
    span_p->add_processing();
}


void
PScore::clean_cols()
{
    int rank_i = 0;
    for (iter_top(col_p_list_,c); c.ok(); )
	if (!c->used_b()) {
	    delete c.remove_p();
	} else {
	    c->set_rank(rank_i++);
	    c++;
	}
}

void
PScore::add(PCol *p)
{
    p->pscore_l_ = this;
    if (p->breakable_b()){
	p->prebreak_p_->pscore_l_ = this;
	p->postbreak_p_->pscore_l_ = this;
    }
    col_p_list_.bottom().add(p);
}

void
PScore::output(Tex_stream &ts)
{
    ts << "\n "<<  paper_l_->lookup_l()->texsetting << "%(Tex id)\n";
    ts<< super_elem_l_->TeX_string();
    ts << "\n\\EndLilyPondOutput";
}


void
PScore::OK()const
{
#ifndef NDEBUG
    for (iter_top(col_p_list_,cc); cc.ok(); cc++)
	cc->OK();
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
    mtor << "\n unbroken spanners: ";
    for (iter(span_p_list_.top(), i); i.ok(); i++)
	i->print();
    mtor << "\ncolumns: ";
     for (iter_top(col_p_list_,cc); cc.ok(); cc++)
	cc->print();
    
    mtor << "}\n";
#endif 
}

void
PScore::preprocess()
{
    super_elem_l_->breakable_col_processing();
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
    
    return col_p_list_.find((PCol*)what);
}


void
PScore::set_breaking(Array<Col_hpositions> const &breaking)
{
    super_elem_l_->line_of_score_l_->set_breaking( breaking);
    super_elem_l_->break_processing();


    for (iter(span_p_list_.top(),i); i.ok(); ) {
	Spanner *span_p = i.remove_p();
	if (span_p->broken_b()) {
	    span_p->unlink();
	    delete span_p;
	}else{
	    typeset_broken_spanner(span_p);
	}
    }
    for (iter(elem_p_list_.top(),i ); i.ok() ;) {
	Item *i_l =i->item();
	if ( i_l && !i_l->pcol_l_->line_l_) {
	    i_l->unlink();
	    delete i.remove_p();
	} else
	    i++;
    }

    for (iter_top(col_p_list_, i); i.ok(); i++)
	i->clean_breakable_items();
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

    PCursor<PCol*> start(l ? find_col(l)+1 : col_p_list_.top() );
    PCursor<PCol*> stop(r ? find_col(r) : col_p_list_.bottom());

    /*
      ugh! windows-suck-suck-suck.
     */
    while ( PCursor<PCol*>::compare(start,stop) < 0 ) {
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
    
    PCursor<PCol*> start(l ? find_col(l)+1 : col_p_list_.top() );
    PCursor<PCol*> stop(r ? find_col(r) : col_p_list_.bottom());
    ret.push(l);
    
    /*
      ugh! windows-suck-suck-suck.
     */
    while ( PCursor<PCol*>::compare(start,stop) < 0 )
	ret.push(start++);
    ret.push(r);
    return ret;
}

Link_array<PCol>
PScore::broken_col_range(PCol*l,PCol*r)const
{
    Link_array<PCol> ret;

    PCursor<PCol*> start(l ? find_col(l)+1 : col_p_list_.top() );
    PCursor<PCol*> stop(r ? find_col(r) : col_p_list_.bottom());
  
    /*
      ugh! windows-suck-suck-suck.
      */
    while ( PCursor<PCol*>::compare(start,stop) < 0 ) {
	if (start->breakable_b() && !start->line_l_ )
	    ret.push(start);
	start++;
    }

    return ret;
}
