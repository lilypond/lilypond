//
// main.cc -- implement silly main() entry point
// should have Root class.
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "mi2mu.hh"

Sources source;
Sources* source_l_g = &source;

Verbose level_ver = NORMAL_ver;

// ugh, another global
String
find_file( String str )
{
    return str;
}

void
usage()
{
    tor( NORMAL_ver ) <<
    	"Usage: mi2mu [options] midi-file\n"
	"Translate midi-file to mudela\n"
	"\n"
	"Options:\n"
	"  -b, --no-quantify      write exact durations, e.g.: a4*385/384\n"
	"  -d, --debug            print lots of debugging stuff\n"
	"  -h, --help             this help\n"
        "  -I, --include=DIR      add DIR to search path\n"
        "  -n, --no-silly         assume no plets or double dots, smallest is 32\n"
	"  -o, --output=FILE      set FILE as default output\n"
	"  -p, --no-plets         assume no plets\n"
	"  -q, --quiet            be quiet\n"
	"  -s, --smallest=N       assume no shorter (reciprocal) durations than N\n"
	"  -v, --verbose          be verbose\n"
	"  -w, --warranty         show warranty and copyright\n"
	"  -x, --no-double-dots   assume no double dotted notes\n"
	;
}

void
identify()
{
	tor( NORMAL_ver ) << mi2mu_version_str() << endl;
}
    
void 
notice()
{
    tor( NORMAL_ver ) <<
	"\n"
	"Mi2mu, translate midi to mudela.\n"
	"Copyright (C) 1997 by\n"
	"  Jan Nieuwenhuizen <jan@digicash.com>\n"
	"  Han-Wen Nienhuys <hanwen@stack.nl>\n"
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
main( int argc_i, char* argv_sz_a[] )
{
	Long_option_init long_option_init_a[] = {
		0, "be-blonde", 'b',
		0, "debug", 'd',
		0, "help", 'h',
		0, "no-silly", 'n',
		1, "output", 'o',
		0, "no-plets", 'p',
		0, "quiet", 'q',
		1, "smallest", 's',
		0, "verbose", 'v',
		0, "warranty", 'w',
		0, "no-double-dots", 'x',
		0,0,0
	};
	Getopt_long getopt_long( argc_i, argv_sz_a, long_option_init_a );
	identify();

	String output_str;
	while ( Long_option_init* long_option_init_p = getopt_long() )
		switch ( long_option_init_p->shortname ) {
		case 'b':
			Duration_convert::no_quantify_b_s = true;
			break;
		case 'd':
			level_ver = DEBUG_ver;
			break;
		case 'h':
			usage();
			exit( 0 );
			break;
//		case 'I':
//			path->push( getopt_long.optarg );
//			break;
		case 'n':
			Duration_convert::no_double_dots_b_s = true;
			Duration_convert::no_triplets_b_s = true;
			Duration_convert::no_smaller_than_i_s = 32;
			break;
		case 'o':
			output_str = getopt_long.optarg;
			break;
		case 'p':
			Duration_convert::no_triplets_b_s = true;
			break;
		case 'q':
			level_ver = QUIET_ver;
			break;
		case 's': {
				int i = String_convert::dec2_i( getopt_long.optarg );
				if ( !i ) {
					usage();
					exit( 2 ); //usage
				}
				Duration_convert::no_smaller_than_i_s = i;
			}
			break;
		case 'v':
			level_ver = VERBOSE_ver;
			break;
		case 'w':
			notice();
			exit( 0 );
			break;
		case 'x':
			Duration_convert::no_double_dots_b_s = false;
			break;
		default:
			assert( 0 );
			break;
  		}
  
	char* arg_sz = 0;
	while ( ( arg_sz = getopt_long.get_next_arg() ) ) {
		My_midi_parser midi_parser( arg_sz, & source );
		midi_parser_l_g = &midi_parser;

		int error_i = midi_parser.parse();
		if ( error_i )
			return error_i;
		if ( !output_str.length_i() ) {
		    String d, dir, base, ext;

		    split_path(arg_sz, d, dir, base, ext);
		    
		    output_str = base + ext + ".ly";
		}
		error_i = midi_parser.output_mudela( output_str );
		if ( error_i )
			return error_i;
		midi_parser_l_g = 0;
	}
	return 0;
}
