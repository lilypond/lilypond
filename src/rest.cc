#include "rest.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper.hh"
#include "lookup.hh"
#include "molecule.hh"


Rest::Rest(int t, int d)
{
    balltype = t;
    dots = d;
}
NAME_METHOD(Rest);

void
Rest::do_print()const
{
#ifndef NPRINT
    mtor << "Rest "<<balltype<< "dots " << dots;
    Item::print();
#endif
}

Molecule*
Rest::brew_molecule_p()const
{
    Paperdef *p =paper();

    Symbol s;
    s = p->lookup_p_->rest(balltype);
    
    Molecule *m = new Molecule(Atom(s));
    if (dots) {
	Symbol d =p->lookup_p_->dots(dots);
	Molecule dm;
	dm.add(Atom(d));
	m->add_right(dm);
    }
    return m;
}

