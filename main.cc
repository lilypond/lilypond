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
    0,0,0
};


void notice()
{
    cout <<
	"LilyPond, a music typesetter.
Copyright (C) 1996 by
  Han-Wen Nienhuys <hanwen@stack.urc.tue.nl>


    This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License version 2
as published by the Free Software Foundation.

    This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

    You should have received a copy (refer to the file COPYING) of the
GNU General Public License along with this program; if not, write to
the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
USA.
"
}

int
main (int argc, char **argv)
{
    Getopt_long oparser(argc, argv,theopts);

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
	default:
	    assert(false);
	    break;
	}
    }
    char *arg = oparser.get_next_arg();

    if (!arg) arg = "";
    parse_file(arg);

    do_scores();
    exit (0);
}
