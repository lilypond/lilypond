#include <iostream.h>
#include <assert.h>
#include "lgetopt.hh"
#include "misc.hh"
#include "string.hh"
#include "main.hh"

extern void parse_file(String s);

long_option_init theopts[] = {
    1, "output", 'o',
    0, "warranty", 'w',
    0, "help", 'h',
    0, "debug", 'd',
    0,0,0
};

void
help()
{
    cout <<
	"--help, -h		This help\n"
	"--warranty, -w		show warranty & copyright\n"
	"--output, -o		set default output\n"
	"--debug, -d		enable debug output\n"
	;
    
}
void notice()
{
    cout <<
	"\n"
	"LilyPond, a music typesetter.\n"
	"Copyright (C) 1996 by\n"
	"  Han-Wen Nienhuys <hanwen@stack.nl>\n"
	"  Jan-Nieuwenhuizen <jan@digicash.com>\n"
	"\n"
	"    This program is free software; you can redistribute it and/or\n"
	"modify it under the terms of the GNU General Public License version 2\n"
	"as published by the Free Software Foundation.\n"
	"\n"
	"    This program is distributed in the hope that it will be useful,\n"
	"but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
	"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n"
	"General Public License for more details.\n"
	"\n"
	"    You should have received a copy (refer to the file COPYING) of the\n"
	"GNU General Public License along with this program; if not, write to\n"
	"the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,\n"
	"USA.\n";
}

int
main (int argc, char **argv)
{
    Getopt_long oparser(argc, argv,theopts);
    debug_init();
    cout << get_version();
    
    while (long_option_init * opt = oparser()) {
	switch ( opt->shortname){
	case 'o':
	    set_default_output(oparser.optarg);
	    break;
	case 'w':
	    notice();
	    exit(0);
	    break;
	case 'h':
	    help();
	    exit(0);
	    break;
	case 'd':
	    set_debug(true);
	    break;
	default:
	    assert(false);
	    break;
	}
    }

    int p=0;
    char *arg ;
    while ( (arg= oparser.get_next_arg()) ) {
	parse_file(arg);	
	do_scores();
	p++;
    }
    if (!p) {
	parse_file("");	
	do_scores();
    }
	
    exit (0);
}
