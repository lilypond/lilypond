#include "string.hh"
#include "molecule.hh"
#include "meter.hh"
#include "paper.hh"
#include "lookup.hh"


Meter::Meter(svec<String> a)
    :args(a)
{
}
void
Meter::preprocess()
{    
    Parametric_symbol *p = paper()->lookup_->meter("general");
    Symbol s = p->eval(args);
    delete p;
    output = new Molecule(Atom(s));
}

