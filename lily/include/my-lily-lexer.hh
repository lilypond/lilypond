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
class My_lily_lexer : public Includable_lexer 
{
public:
  String main_input_str_;
  void * lexval_l;
  Scope * toplevel_scope_p_;
  bool main_input_b_;

  Notename_table *chordmodifier_tab_p_;
  Notename_table *note_tab_p_;
  Link_array<Scope> scope_l_arr_;
  Keyword_table * keytable_p_;
  int errorlevel_i_;

  My_lily_lexer ();
  ~My_lily_lexer ();
  int yylex ();

  void start_main_input ();
  bool notename_b (String) const;
  bool chordmodifier_b (String) const;
  void set_chordmodifier_table (Notename_table*tab_p);
  void set_notename_table (Notename_table*tab_p);
  Identifier*lookup_identifier (String s);
  Musical_pitch lookup_notename (String s);
  Musical_pitch lookup_chordmodifier (String s);
  void push_note_state();
  void push_chord_state();
  void push_lyric_state();
  void pop_state();
  void LexerError (char const *);
  void set_identifier (String str, Identifier* i, bool unique_b = true);
  void print_declarations (bool init_b) const;
  bool note_state_b() const;
  bool chord_state_b() const;
  bool lyric_state_b() const;

private:
  int lookup_keyword (String);
  int scan_bare_word (String);
  int scan_escaped_word (String);

  char escaped_char(char) const;
};

#endif
