#include "bar.hh"
#include "string.hh"
#include "molecule.hh"
#include "paperdef.hh"
#include "lookup.hh"
#include "debug.hh"

NAME_METHOD(Bar);

Bar::Bar( String t)
{
    type = t;
}

void
Bar::do_print()const
{
    mtor << type;
}

Molecule*
Bar::brew_molecule_p()const
{    
    Symbol s = paper()->lookup_p_->bar(type);
    Molecule*output = new Molecule(Atom(s));
    return output;
}
    
