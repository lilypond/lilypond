#include "scalar.hh"
#include "molecule.hh"
#include "meter.hh"
#include "paper.hh"
#include "lookup.hh"

Meter::Meter(svec<Scalar>a)
    :args(a)
{
}

Molecule*
Meter::brew_molecule()const
{
    Symbol s = paper()->lookup_->meter(args);
return new Molecule(Atom(s));
}

