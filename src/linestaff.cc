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
    Atom a  = pscore_l_->paper_l_->lookup_p_->linestaff(nolines,width);
    stafsym_p_ = new Molecule(a);
}







