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
Bar::brew_molecule_p()const
{    
    Symbol s = paper()->lookup_p_->bar(type);
Molecule*    output = new Molecule(Atom(s));
return output;
    
}
    
