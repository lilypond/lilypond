/*
  lexer.hh -- declare My_lily_lexer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef LEXER_HH
#define LEXER_HH

#include <FlexLexer.h>

#include "dictionary.hh"
#include "lily-proto.hh"
#include "lily-proto.hh"
#include "fproto.hh"
#include "array.hh"
#include "string.hh"
#include "includable-lexer.hh"
#include "duration.hh"
#include "musical-pitch.hh"

bool busy_parsing();
void kill_lexer();
void set_lexer();


/// lexer for Mudela
class My_lily_lexer : public Includable_lexer {
  int lookup_keyword (String);
  int scan_bare_word (String);
  int scan_escaped_word (String);

  char escaped_char(char) const;
public:
  String main_input_str_;
  void * lexval_l;
  Scope * toplevel_scope_p_;
  
  Notename_table *note_tab_p_;
  Array<Scope*> scope_l_arr_;
  Keyword_table * keytable_p_;
  int errorlevel_i_;


  void start_main_input ();
  void set_notename_table(Notename_table*tab_p);
  bool notename_b(String) const;
  Identifier*lookup_identifier (String s);
  Musical_pitch lookup_pitch (String s);
  void push_note_state();
  void push_lyric_state();
  void pop_state();
  void LexerError (char const *);
  My_lily_lexer();
  void set_identifier (String str, Identifier* i, bool unique_b = true);
  ~My_lily_lexer();
  int yylex();
  void print_declarations (bool init_b) const;
  void add_notename (String, Musical_pitch);
  bool note_state_b() const;
  bool lyric_state_b() const;
};

#endif
