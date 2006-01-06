/*
  lexer.hh -- declare Lily_lexer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef MY_LILY_LEXER_HH
#define MY_LILY_LEXER_HH

#include "includable-lexer.hh"

#include "input.hh"
#include "duration.hh"
#include "pitch.hh"

bool busy_parsing ();
void kill_lexer ();
void set_lexer ();

class Lily_lexer : public Includable_lexer
{
  DECLARE_SMOBS (Lily_lexer,);

private:
  int lookup_keyword (String);
  int scan_bare_word (String);
  SCM scan_markup_word (String);
  int scan_escaped_word (String);
  int identifier_type (SCM);
  char escaped_char (char) const;

  Keyword_table *keytable_;
  SCM scopes_;
  SCM start_module_;
public:
  String main_input_name_;
  void *lexval;
  Input *lexloc;
  bool is_main_input_;

  Sources *sources_;

  /* Scheme hash tables with (oct name acc)  values, and symbol keys.  */
  SCM chordmodifier_tab_;
  SCM pitchname_tab_stack_;

  int error_level_;
  Input last_input_;

  Lily_lexer (Sources *);
  Lily_lexer (Lily_lexer const &);
  int yylex ();

  void add_lexed_char (int);

  void prepare_for_next_token ();
  int try_special_identifiers (SCM *, SCM);
  Input here_input () const;

  void add_scope (SCM);
  SCM set_current_scope ();
  SCM remove_scope ();

  void start_main_input ();

  SCM lookup_identifier (String s);
  SCM lookup_identifier_symbol (SCM s);
  void push_chord_state (SCM tab);
  void push_figuredbass_state ();
  void push_lyric_state ();
  void push_initial_state ();
  void push_markup_state ();
  void push_note_state (SCM tab);
  void pop_state ();
  void LexerError (char const *);
  void set_identifier (SCM name_string, SCM);
  bool is_note_state () const;
  bool is_chord_state () const;
  bool is_lyric_state () const;
  bool is_figure_state () const;
};

#endif /* MY_LILY_LEXER_HH */
