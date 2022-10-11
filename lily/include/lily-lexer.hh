/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef LILY_LEXER_HH
#define LILY_LEXER_HH

#include "includable-lexer.hh"

#include "input.hh"
#include "duration.hh"
#include "pitch.hh"
#include "protected-scm.hh"

class Lily_lexer : public Smob<Lily_lexer>, public Includable_lexer
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  int scan_word (SCM &output, SCM sym);

private:
  int lookup_keyword (SCM);
  int scan_bare_word (const std::string &);
  int scan_escaped_word (const std::string &);
  int scan_shorthand (const std::string &);
  int scan_scm_id (SCM);
  int identifier_type (SCM);
  void push_markup_predicates (SCM sig);
  char escaped_char (char) const;
  const char *YYText_utf8 ();

  Lily_parser *parser_;
  static Protected_scm keytable_;
  SCM scopes_;
  SCM start_module_;
  Input override_input_;
  SCM eval_scm (SCM, Input, char extra_token = 0);

public:
  SCM eval_scm_token (SCM sval, Input w)
  {
    w.step_forward ();
    return eval_scm (sval, w, '#');
  }
  SCM extra_tokens_;
  SCM *lexval_;
  Input *lexloc_;
  bool is_main_input_;
  vsize main_input_level_;

  Sources *sources_;

  /* Scheme hash tables with (oct name acc)  values, and symbol keys.  */
  SCM chordmodifier_tab_;
  SCM pitchname_tab_stack_;

  int error_level_;
  Input last_input_;

  Lily_lexer (Sources *, Lily_parser *);
  Lily_lexer (Lily_lexer const &, Lily_parser *, SCM);
  int yylex () override;

  void add_lexed_char (int);

  void prepare_for_next_token ();
  int try_special_identifiers (SCM *, SCM);
  Input here_input () const;
  Input const &override_input (Input const &) const;

  void add_scope (SCM);
  SCM set_current_scope ();
  bool has_scope () const;
  SCM remove_scope ();

  void start_main_input ();

  bool top_input () { return include_stack_.size () < 2; }
  SCM lookup_identifier_symbol (SCM s);
  void push_extra_token (Input const &where, int token_type,
                         SCM scm = SCM_UNSPECIFIED);
  int pop_extra_token ();
  void push_chord_state ();
  void push_figuredbass_state ();
  void push_lyric_state ();
  void push_initial_state ();
  void push_markup_state ();
  void push_drum_state ();
  void push_note_state ();
  void push_pitch_names (SCM alist);
  void pop_state ();
  void LexerError (char const *) override;
  void LexerWarning (char const *);
  void set_identifier (SCM path, SCM val);
  int get_state () const;
  bool is_note_state () const;
  bool is_chord_state () const;
  bool is_lyric_state () const;
  bool is_clean () const;
};

#endif /* LILY_LEXER_HH */
