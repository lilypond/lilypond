/*
  lexer.hh -- declare My_lily_lexer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#ifndef LEXER_HH
#define LEXER_HH

#include <FlexLexer.h>

#include "dictionary.hh"
#include "lily-proto.hh"
#include "flower-proto.hh"
#include "array.hh"
#include "string.hh"
#include "includable-lexer.hh"
#include "duration.hh"
#include "pitch.hh"
#include "protected-scm.hh"

bool busy_parsing ();
void kill_lexer ();
void set_lexer ();

/// lexer for Lilypond
class My_lily_lexer : public Includable_lexer 
{
public:
  String main_input_str_;
  void * lexval_l;
  Scheme_hash_table * toplevel_variable_tab_;
  Scope * scope_p_;
  
  bool main_input_b_;

  /*
    Scheme hash tables with (oct name acc)  values, and symbol keys
   */
  Protected_scm chordmodifier_tab_;
  Protected_scm pitchname_tab_;
  
  Link_array<Scope> scope_l_arr_;
  Keyword_table * keytable_p_;
  int errorlevel_i_;

  My_lily_lexer ();
  ~My_lily_lexer ();
  int yylex ();

  Input here_input () const;

  void start_main_input ();

  SCM lookup_identifier (String s);
  void push_note_state ();
  void push_figuredbass_state ();
  void push_chord_state ();
  void push_lyric_state ();
  void pop_state ();
  void LexerError (char const *);
  void set_identifier (String str, SCM);
  bool note_state_b () const;
  bool chord_state_b () const;
  bool lyric_state_b () const;
  bool figure_state_b () const;
private:
  int lookup_keyword (String);
  int scan_bare_word (String);
  int scan_escaped_word (String);

  char escaped_char (char) const;
};

#endif
