#include "idealspacing.hh"
#include "debug.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "dimen.hh"
#include "scoreline.hh"
#include "pscore.hh"
#include "tex-stream.hh"
#include "item.hh"
#include "break.hh"

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
    for (iter_top(cols,c); c.ok(); )
	if (!c->used_b()) {
	    delete c.remove_p();
	} else
	    c++;
}


void
PScore::add(PStaff *s)
{
    assert(s->pscore_l_ == this);
    staffs.bottom().add(s);
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
PScore::typeset_item(Item *i, PCol *c, PStaff *s, int breakstat)
{
    assert(c && i && s);

    if (breakstat == 0) {
	typeset_item(i, c->prebreak_p_, s);
	return;
    }

    if (breakstat == 2) {
	typeset_item(i, c->postbreak_p_, s);
	return;
    }


    its.bottom().add(i);
    s->add(i);
    c->add(i);

    /* first do this, because i->width() may follow the 0-pointer */
    i->add_processing();    
}

void
PScore::typeset_spanner(Spanner*span_p, PStaff*ps)
{
    span_p->pstaff_l_ = ps;
    spanners.bottom().add(span_p);
    ps->spans.bottom().add(span_p);

    // do not init start/stop fields. These are for broken spans only.
    span_p->add_processing();
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

PScore::PScore( Paper_def*p)
{
    paper_l_ = p;
}

void
PScore::output(Tex_stream &ts)
{
    int l=1;

    ts << "\n "<<  paper_l_->lookup_l()->texsetting << "%(Tex id)\n";
    for (iter_top(lines,lic); lic.ok(); lic++) {
	ts << "% line of score no. " << l++ <<"\n";
	ts << lic->TeXstring();
	if ((lic+1).ok())
	    ts << "\\interscoreline\n";
    }	
}


Array<Item*>
PScore::select_items(PStaff*ps, PCol*pc)
{
    Array<Item*> ret;
    assert(ps && pc);
    for (iter_top(pc->its,i); i.ok(); i++){
	if (i->pstaff_l_ == ps)
	    ret.push((Item*)(const Item*)i);
    }
    return ret;
}

void
PScore::OK()const
{
#ifdef NDEBUG
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
    for (iter_top(spanners,i); i.ok(); i++) {
	i->pre_processing();
    }
    for (iter_top(its,i); i.ok(); i++){
	i->pre_processing();
    }
}

void
PScore::postprocess()
{
    for (iter_top(broken_spans,i); i.ok(); i++) { // could chase spans as well.
	i->post_processing();
    }
    for (iter_top(its,i); i.ok(); i++){
	i->post_processing();
    }
    
    for (iter_top(broken_spans,i); i.ok(); i++) {
	i->molecule_processing();
    }
    for (iter_top(its,i); i.ok(); i++){
	i->molecule_processing();
    }

    for (iter_top(lines,i); i.ok(); i++)
	i->process();

}

PCursor<PCol *>
PScore::find_col(const PCol *c)const
{
    const PCol*what = c;
    if (what->daddy_l_ )
	what = what->daddy_l_;
    
    return cols.find((PCol*)what);
}

void
PScore::add_broken(Spanner*s)
{
    assert(s->left->line_l_ == s->right->line_l_);
    broken_spans.bottom().add(s);
    s->left->starters.bottom().add (s);
    s->right->stoppers.bottom().add (s);
}

void
PScore::set_breaking(Array<Col_hpositions> breaking)
{
    for (int j=0; j < breaking.size(); j++) {
	Array<PCol*> &curline(breaking[j].cols);
	Array<Real> &config(breaking[j].config);
	
	Line_of_score *s_p = new Line_of_score(curline,this);
	lines.bottom().add(s_p);   	
	for (int i=0; i < curline.size(); i++){
	    curline[i]->hpos = config[i];
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
    *mlog << "Preprocessing ... " <<flush;
    preprocess();
    *mlog << "\nCalculating column positions ... " <<flush;
    calc_breaking();
    *mlog << "\nPostprocessing ..." << endl;
    postprocess();
}
