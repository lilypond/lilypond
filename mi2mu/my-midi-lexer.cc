//
// my-midi-lexer.cc -- implement My_midi_lexer
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "mi2mu.hh"

int
yylex() 
{
    return midi_lexer_l_g->yylex();
}

My_midi_lexer* midi_lexer_l_g = 0;

My_midi_lexer::My_midi_lexer( String &filename_str, Sources * sources )
{
    source_file_p_ =sources->get_file_l(filename_str);
    switch_streams( source_file_p_->istream_l() );
    errorlevel_i_ = 0;
    char_count_ = 0;
}

My_midi_lexer::~My_midi_lexer()
{
    delete source_file_p_;
}

void
My_midi_lexer::error( char const* sz_l )
{
    if ( !source_file_p_ ) {
	cerr << "error at EOF" << sz_l << '\n';
    } else {
	char const* ch_C = here_ch_C();
	if ( ch_C ) {
	    ch_C--;
	    while ( ( *ch_C == ' ' ) || ( *ch_C == '\t' ) || ( *ch_C == '\n' ) )
		ch_C--;
	    ch_C++;
	}
	errorlevel_i_ |= 1;
	::error( sz_l, ch_C );
    }
}

char const*
My_midi_lexer::here_ch_C()
{
    return source_file_p_->ch_C() + char_count_ ;
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

