/*
  p-score.cc -- implement Paper_score

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
#include "p-col.hh"

#include "word-wrap.hh"
#include "gourlay-breaking.hh"

Paper_score::Paper_score(Paper_def*p)
{
    paper_l_ = p;
    super_elem_l_   = new Super_elem;
    typeset_element(super_elem_l_);
}

Paper_score::~Paper_score()
{
    super_elem_l_->unlink_all();
}

void
Paper_score::typeset_element(Score_elem * elem_p)
{
    elem_p_list_.bottom().add(elem_p);
    elem_p->pscore_l_ = this;

    elem_p->add_processing();
}

void
Paper_score::typeset_item(Item *i, PCol *c)
{
    assert(c && i);
    int breakstat = i->break_status_i_;

    if (breakstat == -1) {
	c = c->prebreak_p_;
    }else if (breakstat == 1) {
	c = c->postbreak_p_;
    }

    c->add(i);
    typeset_element(i);
}

void
Paper_score::typeset_broken_spanner(Spanner*span_p)
{
    span_p->left_col_l_->starters.bottom().add (span_p);
    assert(span_p->left_col_l_->line_l_ == span_p->right_col_l_->line_l_);

    typeset_element(span_p);
}


void
Paper_score::typeset_unbroken_spanner(Spanner*span_p)
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
Paper_score::clean_cols()
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
Paper_score::add(PCol *p)
{
    p->pscore_l_ = this;
    if (p->breakable_b()){
	p->prebreak_p_->pscore_l_ = this;
	p->postbreak_p_->pscore_l_ = this;
    }
    col_p_list_.bottom().add(p);
}

void
Paper_score::output(Tex_stream &ts)
{
    ts << "\n "<<  paper_l_->lookup_l()->texsetting << "%(Tex id)\n";
    ts<< super_elem_l_->TeX_string();
    ts << "\n\\EndLilyPondOutput";
}


void
Paper_score::OK()const
{
#ifndef NDEBUG
    for (iter_top(col_p_list_,cc); cc.ok(); cc++)
	cc->OK();
    for (PCursor<Score_elem*> i( elem_p_list_.top()); i.ok(); i++) 
	i->OK();
#endif
}

void
Paper_score::print() const
{    
#ifndef NPRINT
    if ( !check_debug)
	return ;
    mtor << "Paper_score { ";
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
Paper_score::preprocess()
{
    super_elem_l_->breakable_col_processing();
    super_elem_l_->pre_processing();
}

void
Paper_score::postprocess()
{
    super_elem_l_->post_processing();
    super_elem_l_->molecule_processing();
}

PCursor<PCol *>
Paper_score::find_col(PCol const *c)const
{
    PCol const *what = c;
    if (what->daddy_l_ )
	what = what->daddy_l_;
    
    return col_p_list_.find((PCol*)what);
}


void
Paper_score::set_breaking(Array<Col_hpositions> const &breaking)
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
Paper_score::calc_breaking()
{
    Break_algorithm *algorithm_p;
    Array<Col_hpositions> sol;
    bool try_wrap = ! paper_l_->get_var( "castingalgorithm");

    if (!try_wrap) {
	algorithm_p = new Gourlay_breaking ;
	algorithm_p->set_pscore(this);
	sol = algorithm_p->solve();
	delete algorithm_p;
	if ( ! sol.size()) { 
	     warning( "Can not solve this casting problem exactly; revert to Word_wrap");
	    try_wrap = true;
	} 
    }
    if  (try_wrap) {
	algorithm_p = new Word_wrap;    
	algorithm_p->set_pscore(this);
	sol = algorithm_p->solve();
	delete algorithm_p;
    }
    set_breaking(sol);
}

void
Paper_score::process()
{
    clean_cols();
    print();
    *mlog << "Preprocessing elements... " <<flush;
    preprocess();
    *mlog << "\nCalculating column positions ... " <<flush;
    calc_breaking();
    *mlog << "\nPostprocessing elements..." << endl;
    postprocess();
    
#ifndef NDEBUGA
    for(PCursor<Score_elem*> i(elem_p_list_.top()); i.ok(); i++) 
	assert (i->status() >= 9);
#endif
}

/** Get all breakable columns between l and r, (not counting l and r).  */
Link_array<PCol>
Paper_score::breakable_col_range(PCol*l,PCol*r)const
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
Paper_score::col_range(PCol*l,PCol*r)const
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
Paper_score::broken_col_range(PCol*l,PCol*r)const
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
