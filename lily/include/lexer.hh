/*
  lexer.hh -- declare My_flex_lexer

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef LEXER_HH
#define LEXER_HH
#include <FlexLexer.h>
#include "proto.hh"
#include "fproto.hh"
#include "varray.hh"
#include "string.hh"

int yylex();
void yyerror(const char *s);
bool busy_parsing();
void kill_lexer();
void set_lexer();


struct Lexer_prefs {
    int default_duration, default_dots, default_octave_i_;
    int default_plet_type, default_plet_dur;
    String textstyle_str_;
    
    bool last_duration_mode ;
    
    Lexer_prefs();
    Moment plet_mom();
    void set_default_duration(int *);
    void set_last_duration(int n);
    void set_duration_mode(String s);
    void get_default_duration(int *);
    void set_plet(int,int);
};

/// lexer with provisions for include files.
struct My_flex_lexer : yyFlexLexer {
    Lexer_prefs prefs;
    
    Array<Input_file*> include_stack_;
    Assoc<String, Identifier*> *identifier_assoc_p_;
    Keyword_table * keytable_p_;
    int errorlevel_i_;

    /* *************** */

    char const* here_ch_c_l();
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
    void print_declarations() const;
};

extern My_flex_lexer *lexer;

#endif
