//
// my-midi-parser.cc -- implement My_midi_parser
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"

void
yyerror(char const* sz_l )
{
	midi_parser_l_g->error( sz_l );
}


My_midi_parser* midi_parser_l_g;

My_midi_parser::My_midi_parser( String filename_str )
{
	midi_lexer_p_ = new My_midi_lexer( filename_str );
	midi_parser_l_g = this;
	defined_ch_c_l_ = 0;
	fatal_error_i_ = 0;
}

My_midi_parser::~My_midi_parser()
{
	delete midi_lexer_p_;
	midi_parser_l_g = 0;
}

void
My_midi_parser::error( char const* sz_l )
{
	midi_lexer_l_g->error( sz_l );

	if ( fatal_error_i_ )
		exit( fatal_error_i_ );
}

int
My_midi_parser::parse()
{
	return ::yyparse();
}

void
My_midi_parser::add_score( Midi_score* midi_score_p )
{
	midi_score_p_array_.push( midi_score_p );
}

