#include "debug.hh"
#include "lexer.hh"
#include "moment.hh"
#include "timedescription.hh"

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
error_t(const String& s, const Moment& r)
{
    String t_mom = String(trunc(r)) +  (r - Moment(trunc(r)));
    String e=s+ "(t = " +  t_mom + ")";
    error(e);
}


void
error_t(const String& s, Time_description const &t_tdes)
{
    String e=s+ "(at " + t_tdes.bars + ": " + t_tdes.whole_in_measure + ")\n";
    error(e);
}
