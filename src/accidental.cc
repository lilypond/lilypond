#include "accidental.hh"
#include "debug.hh"
#include "molecule.hh"
#include "paper.hh"
#include "lookup.hh"

Accidental::Accidental(int t, int p)
{
    type = t;
    pos = p;
}

void
Accidental::preprocess()
{
    brew_molecole();
}

void
Accidental::brew_molecole()
{    
    Symbol s =paper()->lookup_->accidental(type);   
    output = new Molecule(Atom(s));
    output->translate(Offset(0, pos * paper()->interline()/2));
}

void
Accidental::print()const
{
    mtor << "Accidental "<<type;
    Item::print();
}
