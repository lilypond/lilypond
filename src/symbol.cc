#include "symbol.hh"
#include "vray.hh"
#include "textdb.hh"

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

/****************************************************************/

Symbol
Parametric_symbol::eval(String args1)const
{
    svec<String> a;
    a.add(args1);
    return eval(a);
}

Symbol
Parametric_symbol::eval(String args1,String arg2)const
{
    svec<String> a;
    a.add(args1);
    a.add(arg2);
    return eval(a);
}

