/*
  lexer.hh -- declare My_lily_lexer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef LEXER_HH
#define LEXER_HH

#include <FlexLexer.h>

#include "lily-proto.hh"
#include "lily-proto.hh"
#include "fproto.hh"
#include "varray.hh"
#include "string.hh"
#include "includable-lexer.hh"
#include "duration.hh"

bool busy_parsing();
void kill_lexer();
void set_lexer();


/// lexer for Mudela
class My_lily_lexer : public Includable_lexer {
  int lookup_keyword (String);
  int scan_bare_word (String);
  int scan_escaped_word (String);

  bool post_quotes_b_;
  char escaped_char(char) const;
public:
  String main_input_str_;
  void * lexval_l;
  
  Notename_table  *note_tab_p_;
  Dictionary<Identifier*> *identifier_p_dict_p_;
  Keyword_table * keytable_p_;
  int errorlevel_i_;

  /* *************** */

  void start_main_input ();
  void clear_notenames();
  Identifier*lookup_identifier (String s);
  Melodic_req* lookup_melodic_req_l (String s);
  void push_note_state();
  void push_lyric_state();
  void pop_state();
  void LexerError (char const *);
  My_lily_lexer();
  void set_identifier (String,Identifier*i);
  ~My_lily_lexer();
  int yylex();
  void print_declarations (bool init_b) const;
  void add_notename (String, Melodic_req*);
  bool note_state_b() const;
  bool lyric_state_b() const;
};

#endif
