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
    Symbol s = paper()->lookup_->meter(args);
    output = new Molecule(Atom(s));
}

