#include <iostream.h>
#include "lgetopt.hh"
#include "misc.hh"
#include "debug.hh"
#include "score.hh"
#include "globvars.hh"

extern void parse_file(String s);
Score *the_score =0;

long_option_init theopts[] = {
    1, "debug", 'd',
    1, "output", 'o',
    0,0,0
};


String outfn="lelie.uit";

void
set_output(String s)
{
    outfn = s;
}

int
main (int argc, char **argv)
{
    Getopt_long oparser(argc, argv,theopts);

    cout << get_version() 
	<< "copyright 1996 Han-Wen Nienhuys\n";
    
    while (long_option_init * opt = oparser()) {
	switch ( opt->shortname){
	case 'd':
	    set_debug(oparser.optarg);
	    break;
	case 'o':
	    set_output(oparser.optarg);
	    break;
	default:
	    assert(false);
	    break;
	}
    }
    char *arg = oparser.get_next_arg();

    if (!arg) arg = "";
    parse_file(arg);

    the_score->process();
    the_score->output(outfn);    
}
