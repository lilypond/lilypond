#include "bar.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper.hh"
#include "lookup.hh"


Bar::Bar( String t)
{
    type = t;
}
void
Bar::preprocess()
{    
    Symbol s = paper()->lookup_->bar(type);
    output = new Molecule(Atom(s));
}
    
