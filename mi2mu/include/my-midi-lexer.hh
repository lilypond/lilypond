//
// my-midi-lexer.hh -- declare My_midi_lexer
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MY_MIDI_LEXER_HH
#define MY_MIDI_LEXER_HH

#include <FlexLexer.h>
#include "proto.hh"
// #include "fproto.hh"
#include "varray.hh"
#include "string.hh"

int yylex();
void yyerror(const char *s);

/// (midi_lexer)
class My_midi_lexer : yyFlexLexer {
public:
	My_midi_lexer( String& filename_str, Sources* );
	~My_midi_lexer();

	int close_i();
	void error( char const* sz_l );
	char const* here_ch_C();
	static int varint2_i( String str );
	int yylex();
	Source_file* source_file_l_ ;

private:
	int char_count_;
	int running_status_i_;

public: // ugh
	int errorlevel_i_;
};

extern My_midi_lexer* midi_lexer_l_g;

#endif // MY_MIDI_LEXER_HH

