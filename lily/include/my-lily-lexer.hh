/*
  lexer.hh -- declare My_lily_lexer

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
void yyerror(char const *s);
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
class My_lily_lexer : public yyFlexLexer {
    int lookup_keyword(String);
    void lookup_notename(int &large, int &small, String s);
    int scan_bare_word(String);
    int scan_escaped_word(String);

public:
    Lexer_prefs prefs;
    
    Array<Input_file*> include_stack_;
    Assoc<String, Identifier*> *identifier_assoc_p_;
    Keyword_table * keytable_p_;
    int errorlevel_i_;

    /* *************** */

    Identifier*lookup_identifier(String s);
    char const* here_ch_c_l();
 
    void push_note_state();
    void push_lyric_state();
    void pop_state();
    void LexerError(char const *);
    String spot() const;
    My_lily_lexer();
    void add_identifier(Identifier*i);
    ~My_lily_lexer();
    void new_input(String s);
    bool  close_input();
    int yylex();
    void print_init_declarations() const;
    void print_user_declarations() const;
    bool note_state_b() const;
    bool lyric_state_b() const;
};

extern My_lily_lexer *lexer;

#endif
