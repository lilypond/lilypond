#include "linestaff.hh"
#include "symbol.hh"
#include "lookupsyms.hh"
#include "dimen.hh"
#include "paper.hh"
#include "pscore.hh"

Linestaff::Linestaff(int l, PScore *s)
    : PStaff(s)
{
    nolines = l;
    stafsym = s->paper_->lookup_->linestaff(l); 
}

Symbol
Linestaff::get_stafsym(Real width)const
{  
   String w(print_dimen(width));
   return stafsym->eval(w);
}
