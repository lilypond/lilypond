#include "debug.hh"
#include "lexer.hh"

ostream &warnout (cerr);
ostream *mlog(&cerr);



void
warning(String s)
{
    WARN << s;
}


void
error(String s)
{
    if (busy_parsing())
	yyerror(s);
    else
	cerr <<  "\nerror: " << s << "\n";
	
    exit(1);
}

void
error_t(String s, Real r)
{
    String e=s+ "(t = " + r + ")";
    error(e);
    exit(1);
}

