#include "symbol.hh"
#include "vray.hh"


Symbol::Symbol()
    :    dim(Interval(0,0),Interval(0,0))
{
    tex = "\\unknown";
}
Symbol::Symbol(String s, Box b)
    :  dim(b)
{
    tex = s;
}


