#include "scalar.hh"
#include "molecule.hh"
#include "meter.hh"
#include "paper-def.hh"
#include "lookup.hh"

Meter::Meter(Array<Scalar>a)
    :args(a)
{
}

Molecule*
Meter::brew_molecule_p()const
{
    Symbol s = paper()->lookup_l()->meter(args);
    return new Molecule(Atom(s));
}

