#include "debug.hh"
#include "lexer.hh"
#include "moment.hh"

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
error_t(const String& s, Moment r)
{
    String e=s+ "(t = " + String(r) + ")";
    error(e);
    exit(1);
}

