#include "musicalrequest.hh"
#include "paper-def.hh"
#include "script.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"



void
Script::set_stem(Stem*st_l)
{
    stem_l_ = st_l;
    add_dependency(st_l);
}

void
Script::set_support(Item*i)
{
    support.push(i);
    add_dependency(i);
}

Script::Script(Script_req* rq, int staflen)
{    
    staffsize =staflen;
    specs_l_ = rq->scriptdef_p_;
    stem_l_ = 0;
    pos = 0;
    symdir=1;
    dir =rq->dir_i_;
}

void
Script::set_symdir()
{
    if (specs_l_->invertsym)
	symdir = (dir < 0) ? -1:1;
}

void
Script::set_default_dir()
{
    if (specs_l_->stemdir) {
	if (!stem_l_)
	    dir = 1;
	else
	    dir = stem_l_->dir * specs_l_->stemdir;
    }
}

Interval
Script::support_height() const return r;
{
    for (int i=0; i < support.size(); i++)
	r.unite(support[i]->height());
}

void
Script::set_default_index()
{
    Real inter_f= paper()->internote();
    Interval dy = symbol().dim.y;
    
    int d = specs_l_->staffdir;
    Real y  ;
    if (!d) {
	Interval v= support_height();
	y = v[dir]  -dy[-dir] + 2*dir*inter_f;
    } else {
	y  = (d > 0) ? staffsize + 2: -2; // ug
	y *=inter_f;
	Interval v= support_height();

	if (d > 0) {
	    y = y >? v.max();
	} else if (d < 0) {
	    y = y <? v.min();
	}
    }
    
    if (stem_l_) {
	Interval v= stem_l_->height();

	if (d > 0 || (!d && dir > 0)) {
	    y = y >? v.max();
	}else if (d < 0 || (!d && dir < 0)) {
	    y = y <? v.min();
	}
    }
    
    pos = int(rint(Real(y)/inter_f));
}

Interval
Script::width() const
{
    return symbol().dim.x;
}

Symbol
Script::symbol()const
{
    String preidx_str = (symdir < 0) ?"-" :"";
    return paper()->lookup_l()->script(preidx_str + specs_l_->symidx);
}

void
Script::do_pre_processing()
{
    set_default_dir();
    set_symdir();
}

void
Script::do_post_processing()
{
    set_default_index();
}

Molecule*
Script::brew_molecule_p() const
{
    Real dy = paper()->internote();
    
    Molecule*out = new Molecule(Atom(symbol()));
    out->translate(Offset(0,dy * pos));
    return out;
}
