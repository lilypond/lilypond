#include "bar.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper.hh"
#include "lookup.hh"


Bar::Bar( String t)
{
    type = t;
}
Molecule*
Bar::brew_molecule()const
{    
    Symbol s = paper()->lookup_->bar(type);
Molecule*    output = new Molecule(Atom(s));
return output;
    
}
    
