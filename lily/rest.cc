/*
  rest.cc -- implement Rest

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "duration.hh"
#include "rest.hh"
#include "dimen.hh" 
#include "debug.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "rest.hh"

Rest::Rest(Duration d)
{
    balltype = d.type_i_;
    dots = d.dots_i_;
    pos_i_ = 0;
}


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
    Paper_def *p =paper();

    Symbol s;
    s = p->lookup_l()->rest(balltype);
    
    Molecule *m = new Molecule(Atom(s));
    if (dots) {
	Symbol d =p->lookup_l()->dots(dots);
	Molecule dm;
	dm.add(Atom(d));
	m->add_right(dm);
    }
    m->translate(Offset(0,pos_i_ * paper()->internote()));
    return m;
}

