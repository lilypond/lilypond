#include "scalar.hh"
#include "molecule.hh"
#include "meter.hh"
#include "paper.hh"
#include "lookup.hh"

Meter::Meter(Array<Scalar>a)
    :args(a)
{
}

Molecule*
Meter::brew_molecule_p()const
{
    Symbol s = paper()->lookup_p_->meter(args);
return new Molecule(Atom(s));
}

