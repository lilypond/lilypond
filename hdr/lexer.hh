#ifndef LEXER_HH
#define LEXER_HH
#include <FlexLexer.h>
#include "proto.hh"
#include "fproto.hh"
#include "sstack.hh"
#include "string.hh"

int yylex();
void yyerror(const char *s);
bool busy_parsing();
void kill_lexer();
void set_lexer();

struct Input_file {
	istream* is;
	Source_file* sourcefile_l_;
	int line;
	String name;

	Input_file(String);
	~Input_file();
};


/// lexer with provisions for include files.
struct My_flex_lexer : yyFlexLexer {

    sstack<Input_file*> include_stack;
    Assoc<String, Identifier*> *the_id_tab;
    Keyword_table * keytable;
    Notename_tab * defaulttab;
    char const* data_ch_c_l_m;
    int errorlevel_i_;
    /****************/
    int ret_notename(int *p, String text, int octave_mod);    
    char const* here_ch_c_l();
    void set(Notename_tab *n);
    int lookup_keyword(String);
    void lookup_notename(int &large, int &small, String s);
    void LexerError(const char *);
    String spot() const;
    Identifier*lookup_identifier(String s);
    My_flex_lexer();
    void add_identifier(Identifier*i);
    ~My_flex_lexer();
    void new_input(String s);
    bool  close_input();
    int yylex();
};

extern My_flex_lexer *lexer;

#endif
