#include "request.hh"
#include "paper.hh"
#include "script.hh"
#include "stem.hh"
#include "molecule.hh"
#include "lookup.hh"

Script::Script(Script_req* rq, Item*i , int staflen, Stem*st_p)
{
    dependencies.add(st_p);
    dependencies.add(i);
    
    staffsize =staflen;
    specs = rq->scriptdef;
    support= i;
    stem_ = st_p;
    pos = 0;
    symdir=1;
    dir =rq->dir;
}

void
Script::set_symdir()
{
    if (specs->invertsym)
	symdir = (dir < 0) ? -1:1;
}

void
Script::set_default_dir()
{
    if (specs->stemdir) {
	if (!stem_)
	    dir = 1;
	else
	    dir = stem_->dir * specs->stemdir;
    }
}

void
Script::set_default_pos()
{
    assert(dir);
    Real y;
    Real inter= paper()->internote();

    int d = specs->staffdir;
    if (!d) {
	Interval v= support->height();
	pos = rint(v[dir]/inter) + dir* 2;
    } else {
	y  = (d > 0) ? staffsize + 2: -2; // ug
	y *=inter;
	Interval v= support->height();

	if (dir > 0) {
	    y = y >? v.max();
	}else if (dir < 0) {
	    y = y <? v.min();
	}
	pos = int(rint(Real(y)/inter));
    }
}

Interval
Script::width() const
{
    return paper()->lookup_->script(specs->symidx).dim.x;
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
    Paperdef *p =paper();

    Real dy = p->internote();
    String preidx = (symdir < 0) ?"-" :"";
    Symbol ss =p->lookup_->script(preidx+specs->symidx);
    Molecule*out = new Molecule(Atom(ss));
    out->translate(Offset(0,dy * pos));
    return
	out;
}
