#include "linestaff.hh"
#include "symbol.hh"



Linestaff::Linestaff(int l)
{
    nolines = l;
    stafsym = Stretchable_symbol::get_linestaff(l); 
}

