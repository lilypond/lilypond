#include "dimen.hh"
#include "tex.hh"
#include "symbol.hh"
#include "const.hh"
#include "vray.hh"

String
vstrut(Real h)
{
    return String("\\vrule height ") + print_dimen(h) + "depth 0pt width 0pt";
}


static void
substitute_arg(String& r, String arg)
{
    int p = r.pos('%');
    if (!p ) return ;
    else p--;
    r = r.left(p) + arg + r.right(r.len() - p -1);
}


String
substitute_args(String source, svec<String> args)    
{
    String retval (source);
    for (int i = 0 ; i < args.sz(); i++)
        substitute_arg(retval, args[i]);
    while (retval.pos('%'))
        substitute_arg(retval, "");
    return retval;
}
String
substitute_args(String source, svec<Scalar> args)    
{
    svec<String> sv;
    for (int i = 0 ; i < args.sz(); i++)
	sv.add(args[i]);
    
    return substitute_args(source, sv);
}
