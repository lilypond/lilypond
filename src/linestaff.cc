#include "linestaff.hh"
#include "molecule.hh"
#include "symbol.hh"
#include "lookup.hh"
#include "dimen.hh"
#include "paper.hh"
#include "pscore.hh"

Linestaff::Linestaff(int l, PScore *s)
    : PStaff(s)
{
    nolines = l;
}

void
Linestaff::brew_molecule(Real width)
{
    Atom a  = pscore_->paper_->lookup_->linestaff(nolines,width);
    stafsym = new Molecule(a);    
}







