#include "idealspacing.hh"
#include "debug.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "paper.hh"
#include "molecule.hh"
#include "dimen.hh"
#include "scoreline.hh"
#include "pscore.hh"
#include "tstream.hh"
#include "item.hh"


Idealspacing*
PScore::get_spacing(PCol*l, PCol*r)
{
    assert(l!=r);
    for (iter_top(suz,ic); ic.ok(); ic++) {
	if (ic->left == l && ic->right == r){
	    return ic;
	}
    }
    
    Idealspacing*ip =new Idealspacing(l,r);
    suz.bottom().add(ip);

    return ip;
}


void
PScore::clean_cols()
{
    for (iter_top(cols,c); c.ok(); )
	if (!c->used()) {
	    c.del();
	} else
	    c++;
}


void
PScore::add(PStaff *s)
{
    assert(s->pscore_ == this);
    staffs.bottom().add(s);
}

void
PScore::typeset_item(Item *i, PCol *c, PStaff *s, int breakstat)
{
    assert(c && i && s);
//    assert(!breakstat != 4 || c->breakable() );
    if (breakstat == 0) {
	typeset_item(i, c->prebreak, s);
	return;
    }

    if (breakstat == 2) {
	typeset_item(i, c->postbreak, s);
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
    span_p->pstaff_ = ps;
    spanners.bottom().add(span_p);
    ps->spans.bottom().add(span_p);

    // do not init start/stop fields. These are for broken spans only.
    span_p->add_processing();
}


int
PScore::compare_pcols(PCol*a,  PCol*b)const
{
    PCursor<PCol*> ac(find_col(a));
    PCursor<PCol*> bc(find_col(b));
    assert(ac.ok() && bc.ok());
    return ac - bc;
}

void
PScore::add(PCol *p)
{
    p->pscore_ = this;
    if (p->breakable()){
	p->prebreak->pscore_ = this;
	p->postbreak->pscore_ = this;
    }
    cols.bottom().add(p);
}

PScore::PScore( Paperdef*p)
{
    paper_ = p;
}

void
PScore::output(Tex_stream &ts)
{
    int l=1;

    ts << "\n "<<  paper_->lookup_->texsetting << "%(Tex id)\n";
    for (iter_top(lines,lic); lic.ok(); lic++) {
	ts << "% line of score no. " << l++ <<"\n";
	ts << lic->TeXstring();
	if ((lic+1).ok())
	    ts << "\\interscoreline\n";
    }	
}


svec<Item*>
PScore::select_items(PStaff*ps , PCol*pc)
{
    svec<Item*> ret;
    assert(ps && pc);
    for (iter_top(pc->its,i); i.ok(); i++){
	if (i->pstaff_ == ps)
	    ret.add((Item*)(const Item*)i);
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
    paper_->print();
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
PScore::find_col(PCol *c)const
{
    PCol*what = (PCol*)c;
    if (what->daddy )
	what = what -> daddy;
    
    return cols.find(what);
}

void
PScore::add_broken(Spanner*s)
{
    assert(s->left->line == s->right->line);
    broken_spans.bottom().add(s);
    s->left->starters.bottom().add (s);
    s->right->stoppers.bottom().add (s);
}

void
PScore::set_breaking(svec<Col_configuration> breaking)
{
    for (int j=0; j < breaking.sz(); j++) {
	svec<PCol*> &curline(breaking[j].cols);
	svec<Real> &config(breaking[j].config);
	
	Line_of_score *p = new Line_of_score(curline,this);
	lines.bottom().add(p);   	
	for (int i=0; i < curline.sz(); i++){
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
