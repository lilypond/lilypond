#include "debug.hh"
#include "script-def.hh"

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
    mtor << "Script_def{ idx: " << symidx 
	 << " direction, stem: " << stemdir << " staff : " << staffdir << "}\n";
}
int
Script_def::compare(Script_def const & c)
{
    return !(symidx == c.symidx &&
	stemdir == c.stemdir&&
	staffdir == c.staffdir&&
	invertsym == c.invertsym);
}
