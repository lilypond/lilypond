//
// my-midi-parser.hh -- declare My_midi_parser
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MY_MIDI_PARSER_HH
#define MY_MIDI_PARSER_HH

#include "proto.hh"
#include "varray.hh"
#include "string.hh"
// #include "plist.hh"

int yyparse();

/// (midi_parser)
class My_midi_parser {
public:
	My_midi_parser( String filename_str );
	~My_midi_parser();
	void add_score( Midi_score* midi_score_p );
	void error( char const* sz_l );
	int parse();

private:
	Array<Midi_score*> midi_score_p_array_;
	char const* defined_ch_c_l_;
	int fatal_error_i_;
	My_midi_lexer* midi_lexer_p_;
};

extern My_midi_parser* midi_parser_l_g;

#endif // MY_MIDI_PARSER_HH

