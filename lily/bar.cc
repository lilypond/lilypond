#include "bar.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "debug.hh"



Bar::Bar( String t)
{
    type = t;
}

IMPLEMENT_STATIC_NAME(Bar);

void
Bar::do_print()const
{
    mtor << type;
}

Molecule*
Bar::brew_molecule_p()const
{    
    Symbol s = paper()->lookup_l()->bar(type);
    Molecule*output = new Molecule(Atom(s));
    return output;
}
    
