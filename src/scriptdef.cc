#include "debug.hh"
#include "scriptdef.hh"

Script_def::Script_def(String idx,  int stem, int staff ,bool invert)
{
    symidx = idx ;
    stemdir =stem;
    staffdir = staff;
    invertsym = invert;
}
void
Script_def::print() const
{
    mtor << "idx: " << symidx;
    mtor << "direction, stem: " << stemdir << " staff : " << staffdir;
}
