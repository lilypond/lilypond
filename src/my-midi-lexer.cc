//
// my-midi-lexer.cc -- implement My_midi_lexer
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "debug.hh"
#include "input-file.hh"
#include "source-file.hh"
#include "my-midi-lexer.hh"

int
yylex() 
{
	return midi_lexer_l_g->yylex();
}

My_midi_lexer* midi_lexer_l_g;

My_midi_lexer::My_midi_lexer( String filename_str )
{
	input_file_p_ = new Input_file( filename_str );
	switch_streams( input_file_p_->is );
	midi_lexer_l_g = this;
	errorlevel_i_ = 0;
}

My_midi_lexer::~My_midi_lexer()
{
	delete input_file_p_;
	midi_lexer_l_g = 0;
}

void
My_midi_lexer::error( char const* sz_l )
{
    if ( !input_file_p_ ) {
//	*mlog << "error at EOF" << sz_l << '\n';
	cerr << "error at EOF" << sz_l << '\n';
    } else {
	char const* ch_c_l = here_ch_c_l();
	if ( ch_c_l ) {
	    ch_c_l--;
	    while ( ( *ch_c_l == ' ' ) || ( *ch_c_l == '\t' ) || ( *ch_c_l == '\n' ) )
		    ch_c_l--;
	    ch_c_l++;
	}
	errorlevel_i_ |= 1;
//	::error( sz_l, ch_c_l );
	::error( sz_l, ch_c_l );
    }
}

char const*
My_midi_lexer::here_ch_c_l()
{
    return input_file_p_->sourcefile_l_->ch_c_l() + yyin->tellg();
}

int
My_midi_lexer::varint2_i( String str )
{
        int var_i = 0;

	for ( int i = 0; i < str.length_i(); i++ ) {
		Byte byte = str[ i ];
		var_i <<= 7;
		var_i += byte & 0x7f;
		if ( ! ( byte & 0x80 ) )
			return var_i;
	}
	cout << "\nvarint2_i:" << String_convert::bin2hex_str( str ) << endl;
	assert( 0 ); // illegal varint
	return 0;
}

int
My_midi_lexer::close_i()
{
	return 0;
}

