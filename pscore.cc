// utility functions for PScore
#include "debug.hh"
#include "molecule.hh"
#include "dimen.hh"
#include "line.hh"
#include "pscore.hh"
#include "tstream.hh"

void
PScore::clean_cols()
{
    for (PCursor<PCol *> c(cols); c.ok(); )
	if (!c->used) {
	    c.del();
	} else
	    c++;
}


void
PScore::add(PStaff *s)
{
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
    if (c->daddy && c == c->daddy->prebreak) { // makeshift.
	Interval iv (i->width());
	if (!iv.empty()) {
	    svec<Item*> col_its (select_items(s, c));
	    for (int j =0; j < col_its.sz(); j++)
		col_its[j]->output->translate(Offset(-iv.length(),0));
	    i->output->translate (Offset(-iv.max, 0));
	}
    }
    its.bottom().add(i);
    s->add(i);
    c->add(i);
}

void
PScore::add_line(svec<const PCol *> curline, svec<Real> config)
{    
    Line_of_score *p = new Line_of_score(curline,this);
    lines.bottom().add(p);   	
    for (int i=0; i < curline.sz(); i++){
	PCol *c=(PCol *)curline[i]; // so, this isn't really const.
	c->hpos= config[i];
    }
}

Idealspacing*
PScore::get_spacing(PCol*l, PCol*r)
{
    assert(l!=r);
    for (PCursor<Idealspacing*> ic (suz); ic.ok(); ic++) {
	if (ic->left == l && ic->right == r){
	    return ic;
	}
    }
    
    Idealspacing*ip =new Idealspacing(l,r);
    suz.bottom().add(ip);

    return ip;
}

svec<const PCol *>
PScore::find_breaks() const
{
    svec<const PCol *> retval;
    for (PCursor<PCol *> c(cols); c.ok(); c++)
	if (c->breakable())
	    retval.add(c);
	    
    return retval;
}

void
PScore::add(PCol *p)
{
    cols.bottom().add(p);
}

PScore::PScore()
{
    linewidth = convert_dimen(15,"cm");	// default
}

void
PScore::output(Tex_stream &ts)
{
    int l=1;
    ts << "% linewidth " << print_dimen(linewidth )+"\n";
    for (PCursor<Line_of_score*> lic(lines); lic.ok(); lic++) {
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
    for (PCursor<const Item*> ic(pc->its); ic.ok(); ic++){
	if (ic->pstaff_ == ps)
	    ret.add((Item*)(const Item*)ic);
    }
    return ret;
}

void
PScore::OK()const
{
#ifdef NDEBUG
    for (PCursor<PCol*> cc(cols); cc.ok(); cc++)
	cc->OK();
    for (PCursor<Idealspacing*> ic(suz); ic.ok(); ic++)
	ic->OK();
#endif
}
void
PScore::print() const
{    
#ifndef NPRINT
    mtor << "PScore { width "<<print_dimen(linewidth);
    mtor << "\ncolumns: ";
    for (PCursor<PCol*> cc(cols); cc.ok(); cc++)
	cc->print();
    
    mtor << "\nideals: ";
    for (PCursor<Idealspacing*> ic(suz); ic.ok(); ic++)
	ic->print();
    mtor << "}\n";
#endif 
}

