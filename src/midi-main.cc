//
// midi-main.cc -- implement silly main() entry point
// should have Root class.
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <iostream.h>
#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "source.hh"
#include "sourcefile.hh"
#include "midi-main.hh"
#include "midi-event.hh"
#include "midi-track.hh"
#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"

Source source;
Source* source_l_g = &source;

//ugh
char const* defined_ch_c_l = 0;

String
find_file( String str )
{
    return str;
}

// ugh, copied from warn.cc, cannot use
void
message( String message_str, char const* context_ch_c_l )
{
    String str = "lilypond: ";
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

int
main( int argc_i, char* argv_sz_a[] )
{
	if ( !argc_i )
		return 2;
	My_midi_parser midi_parser( argv_sz_a[ 1 ] );
	return midi_parser.parse();
}
