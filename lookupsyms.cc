#include "lookupsyms.hh"
#include "debug.hh"
#include "symtable.hh"
#include "dimen.hh"
#include "tex.hh"

Symtables the_sym_tables("symbol.ini");

Symbol
Lookup::ball(int j)
{
    if (j > 4) j = 4;
    Symtable * st = the_sym_tables("balls");
    return st->lookup(String(j));
}

Symbol
Lookup::rest(int j)
{
    return the_sym_tables("rests")->lookup(String(j));
}


 Symbol
Lookup::bar(String s)
{
    return the_sym_tables("bars")->lookup(s);
}
 Symbol
Lookup::dots(int j)
{
    if (j>3)
	error("max 3 dots");
    return the_sym_tables("dots")->lookup(j);
}

/****************************************************************/
// bare bones.

struct Linestaf_symbol : Parametric_symbol {
    int lines;
    Linestaf_symbol(int n) { lines = n;}
    Symbol eval(svec<String>)const;
};


Symbol
Linestaf_symbol::eval(svec<String> w)const
{
    Real wid = w[0].fvalue();

    Symbol s;
    s.dim.x = Interval(0,wid);
    s.dim.y = Interval(0, lines*convert_dimen(5,"pt"));
    svec<String> a;
    a.add(lines);
    a.add(w[0]);
    s.tex = the_sym_tables("param")->lookup("linestaf").tex;
    s.tex = substitute_args(s.tex, a);
    return s;
}

/****************************************************************
 */


struct Meter_sym:Parametric_symbol {

    Symbol eval(svec<String> a) const{
	Symbol s;
	s.dim.x = Interval( convert_dimen(-5,"pt"), convert_dimen(10,"pt"));
	s.dim.y = Interval(0, convert_dimen(10,"pt") );	// todo
	String src = the_sym_tables("param")->lookup("meter").tex;
	s.tex = substitute_args(src,a);
	return s;
    }
};
/****************************************************************/

Parametric_symbol *
Lookup::meter(String )
{
    return new Meter_sym;
}

Parametric_symbol *
Lookup::linestaff(int n)
{
    return new Linestaf_symbol(n);
}

