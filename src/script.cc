#include "request.hh"
#include "paper.hh"
#include "script.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"

Script::Script(Script_req* rq, Item*i , int staflen, Stem*st_l)
{
    dependencies.add(st_l);
    dependencies.add(i);
    
    staffsize =staflen;
    specs_l_ = rq->scriptdef;
    support= i;
    stem_l_ = st_l;
    pos = 0;
    symdir=1;
    dir =rq->dir;
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

void
Script::set_default_pos()
{
    Real inter_f= paper()->internote();
    Interval dy = symbol().dim.y;
    
    int d = specs_l_->staffdir;
    Real y  ;
    if (!d) {
	Interval v= support->height();
	y = v[dir]  -dy[-dir] + 2*dir*inter_f;
    } else {
	Real y  = (d > 0) ? staffsize + 2: -2; // ug
	y *=inter_f;
	Interval v= support->height();

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
    return paper()->lookup_p_->script(preidx_str + specs_l_->symidx);
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
    set_default_pos();
}

Molecule*
Script::brew_molecule() const
{
    Real dy = paper()->internote();
    
    Molecule*out = new Molecule(Atom(symbol()));
    out->translate(Offset(0,dy * pos));
    return out;
}
