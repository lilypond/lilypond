#include "scalar.hh"
#include "molecule.hh"
#include "meter.hh"
#include "paper.hh"
#include "lookup.hh"

Meter::Meter(svec<Scalar>a)
    :args(a)
{
}

void
Meter::preprocess()
{
    Symbol s = paper()->lookup_->meter(args);
    output = new Molecule(Atom(s));
}

