//
// midi-main.cc -- implement silly main() entry point
// should have Root class.
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "m2m.hh"
#include "fversion.hh"
#include "version.hh"

Source source;
Source* source_l_g = &source;

Verbose level_ver = NORMAL_ver;

// ugh
bool no_triplets_bo_g = false;

//ugh
char const* defined_ch_c_l = 0;

// ugh, another global
String
find_file( String str )
{
    return str;
}

// ugh, copied from warn.cc, cannot use
void
message( String message_str, char const* context_ch_c_l )
{
    String str = "m2m: ";
    Source_file* sourcefile_l = source_l_g->sourcefile_l( context_ch_c_l );
    if ( sourcefile_l ) {
	str += sourcefile_l->file_line_no_str(context_ch_c_l) + String(": ");
    }
    str += message_str;
    if ( sourcefile_l ) {
	str += ":\n";
	str += sourcefile_l->error_str( context_ch_c_l );
    }
//    if ( busy_parsing() )
//    	cerr << endl;
    cerr << str << endl;
}

void
warning( String message_str, char const* context_ch_c_l )
{
    message( "warning: " + message_str, context_ch_c_l );
}

void
error( String message_str, char const* context_ch_c_l )
{
    message( message_str, context_ch_c_l );
    // since when exits error again?
    // i-d say: error: errorlevel |= 1; -> no output upon error
    //          warning: recovery -> output (possibly wrong)
    if ( midi_lexer_l_g )
        midi_lexer_l_g->errorlevel_i_ |= 1;
}

void
help()
{
    btor <<
	"--debug, -d		be really verbose\n"
	"--help, -h		this help\n"
        "--include=DIR, -I DIR	add DIR to search path\n"
        "--no-triplets, -n	assume no triplets\n"
	"--output=FILE, -o FILE	set FILE as default output\n"
	"--quiet, -q		be quiet\n"
	"--verbose, -v		be verbose\n"
	"--warranty, -w		show warranty & copyright\n"
	;
}

void
identify()
{
	mtor << version_str() << endl;
}
    
void 
notice()
{
    mtor <<
	"\n"
	"M2m, translate midi to mudela.\n"
	"Copyright (C) 1997 by\n"
	"  Han-Wen Nienhuys <hanwen@stack.nl>\n"
//	"Contributors\n"
	"  Jan Nieuwenhuizen <jan@digicash.com>\n"
//	"  Mats Bengtsson <matsb@s3.kth.se>\n"
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

// should simply have Root class...
String
version_str()
{
	return String ( "This is m2m " ) + VERSIONSTR 
		+ "/FlowerLib " + FVERSIONSTR
		+ " of " +  __DATE__ + " " + __TIME__;
}

int
main( int argc_i, char* argv_sz_a[] )
{
	Long_option_init long_option_init_a[] = {
		0, "debug", 'd',
		0, "help", 'h',
//		1, "include", 'I',
		0, "no-triplets", 'n',
		1, "output", 'o',
		0, "quiet", 'q',
		0, "verbose", 'v',
		0, "warranty", 'w',
		0,0,0
	};
	Getopt_long getopt_long( argc_i, argv_sz_a, long_option_init_a );
	identify();

	String output_str;
	while ( Long_option_init* long_option_init_p = getopt_long() )
		switch ( long_option_init_p->shortname ) {
			case 'd':
				level_ver = DEBUG_ver;
				break;
			case 'h':
				help();
				exit( 0 );
				break;
//			case 'I':
//				path->push( getopt_long.optarg );
//				break;
			case 'n':
				no_triplets_bo_g = true;
				break;
			case 'o':
				output_str = getopt_long.optarg;
				break;
			case 'q':
				level_ver = QUIET_ver;
				break;
			case 'v':
				level_ver = VERBOSE_ver;
				break;
			case 'w':
				notice();
				exit( 0 );
				break;
			default:
				assert( 0 );
				break;
		}

	char* arg_sz = 0;
	while ( ( arg_sz = getopt_long.get_next_arg() ) ) {
		My_midi_parser midi_parser( arg_sz );
		int error_i = midi_parser.parse();
		if ( error_i )
			return error_i;
		if ( !output_str.length_i() ) {
			output_str = String( arg_sz ) + ".ly";
			// i-m sure there-s already some routine for this
			int name_i; // too bad we can-t declare local to if
			if ( ( name_i = output_str.index_last_i( '/' ) ) != -1 )
				output_str = output_str.mid_str( name_i + 1, INT_MAX );
		}
		error_i = midi_parser.output_mudela( output_str );
		if ( error_i )
			return error_i;
	}
	return 0;
}
