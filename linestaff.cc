#include "linestaff.hh"
#include "symbol.hh"
#include "lookupsyms.hh"
#include "dimen.hh"

Linestaff::Linestaff(int l)
{
    nolines = l;
    stafsym = Lookup::linestaff(l); 
}

Symbol
Linestaff::get_stafsym(Real width)const
{  
   String w(print_dimen(width));
   return stafsym->eval(w);
}
