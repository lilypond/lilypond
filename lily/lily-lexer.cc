/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "lily-lexer.hh"

#include <cctype>
#include <sstream>
using namespace std;

#include "context.hh" // for nested_property_alist
#include "international.hh"
#include "interval.hh"
#include "keyword.hh"
#include "main.hh"
#include "moment.hh"
#include "parser.hh"
#include "scm-hash.hh"
#include "source-file.hh"
#include "warn.hh"
#include "program-option.hh"
#include "lily-parser.hh"
#include "ly-module.hh"

static Keyword_ent the_key_tab[]
=
{
  {"accepts", ACCEPTS},
  {"addlyrics", ADDLYRICS},
  {"alias", ALIAS},
  {"alternative", ALTERNATIVE},
  {"book", BOOK},
  {"bookpart", BOOKPART},
  {"change", CHANGE},
  {"chordmode", CHORDMODE},
  {"chords", CHORDS},
  {"consists", CONSISTS},
  {"context", CONTEXT},
  {"default", DEFAULT},
  {"defaultchild", DEFAULTCHILD},
  {"denies", DENIES},
  {"description", DESCRIPTION},
  {"drummode", DRUMMODE},
  {"drums", DRUMS},
  {"etc", ETC},
  {"figuremode", FIGUREMODE},
  {"figures", FIGURES},
  {"header", HEADER},
  {"layout", LAYOUT},
  {"lyricmode", LYRICMODE},
  {"lyrics", LYRICS},
  {"lyricsto", LYRICSTO},
  {"markup", MARKUP},
  {"markuplist", MARKUPLIST},
  {"midi", MIDI},
  {"name", NAME},
  {"new", NEWCONTEXT},
  {"notemode", NOTEMODE},
  {"override", OVERRIDE},
  {"paper", PAPER},
  {"remove", REMOVE},
  {"repeat", REPEAT},
  {"rest", REST},
  {"revert", REVERT},
  {"score", SCORE},
  {"sequential", SEQUENTIAL},
  {"set", SET},
  {"simultaneous", SIMULTANEOUS},
  {"tempo", TEMPO},
  {"type", TYPE},
  {"unset", UNSET},
  {"with", WITH},
  {0, 0}
};

Lily_lexer::Lily_lexer (Sources *sources, Lily_parser *parser)
{
  parser_ = parser;
  keytable_ = new Keyword_table (the_key_tab);
  chordmodifier_tab_ = SCM_EOL;
  pitchname_tab_stack_ = SCM_EOL;
  sources_ = sources;
  scopes_ = SCM_EOL;
  error_level_ = 0;
  is_main_input_ = false;
  main_input_level_ = 0;
  start_module_ = SCM_EOL;
  extra_tokens_ = SCM_EOL;
  smobify_self ();

  add_scope (ly_make_module (false));
  push_note_state (SCM_EOL);
  chordmodifier_tab_ = scm_make_vector (scm_from_int (1), SCM_EOL);
}

Lily_lexer::Lily_lexer (Lily_lexer const &src, Lily_parser *parser,
                        SCM override_input)
  : Includable_lexer ()
{
  parser_ = parser;
  keytable_ = (src.keytable_) ? new Keyword_table (*src.keytable_) : 0;
  chordmodifier_tab_ = src.chordmodifier_tab_;
  pitchname_tab_stack_ = src.pitchname_tab_stack_;
  sources_ = src.sources_;
  scopes_ = src.scopes_;
  start_module_ = SCM_EOL;

  error_level_ = 0;
  is_main_input_ = src.is_main_input_;
  main_input_level_ = 0;

  extra_tokens_ = SCM_EOL;
  if (unsmob<Input> (override_input))
    override_input_ = *unsmob<Input> (override_input);

  smobify_self ();

  push_note_state (SCM_EOL);
}

Lily_lexer::~Lily_lexer ()
{
  delete keytable_;
}

void
Lily_lexer::add_scope (SCM module)
{
  ly_reexport_module (scm_current_module ());
  if (!scm_is_pair (scopes_))
    start_module_ = scm_current_module ();

  for (SCM s = scopes_; scm_is_pair (s); s = scm_cdr (s))
    ly_use_module (module, scm_car (s));
  scopes_ = scm_cons (module, scopes_);

  set_current_scope ();
}
bool
Lily_lexer::has_scope () const
{
  return scm_is_pair (scopes_);
}

SCM
Lily_lexer::remove_scope ()
{
  SCM sc = scm_car (scopes_);
  scopes_ = scm_cdr (scopes_);
  set_current_scope ();
  return sc;
}

SCM
Lily_lexer::set_current_scope ()
{
  SCM old = scm_current_module ();

  if (scm_is_pair (scopes_))
    scm_set_current_module (scm_car (scopes_));
  else
    scm_set_current_module (start_module_);

  return old;
}

int
Lily_lexer::lookup_keyword (const string &s)
{
  return keytable_->lookup (s.c_str ());
}

SCM
Lily_lexer::keyword_list () const
{
  if (!keytable_)
    return SCM_EOL;

  SCM l = SCM_EOL;
  SCM *tail = &l;
  for (vsize i = 0; i < keytable_->table_.size (); i++)
    {
      *tail = scm_acons (scm_from_utf8_string (keytable_->table_[i].name_),
                         scm_from_int (keytable_->table_[i].tokcode_),
                         SCM_EOL);

      tail = SCM_CDRLOC (*tail);
    }

  return l;
}

SCM
Lily_lexer::lookup_identifier_symbol (SCM sym)
{
  for (SCM s = scopes_; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM var = ly_module_lookup (scm_car (s), sym);
      if (scm_is_true (var))
        return scm_variable_ref (var);
    }

  return SCM_UNDEFINED;
}

SCM
Lily_lexer::lookup_identifier (const string &name)
{
  return lookup_identifier_symbol (ly_symbol2scm (name.c_str ()));
}

void
Lily_lexer::start_main_input ()
{
  yy_flex_debug = get_program_option ("debug-lexer");
  parser_->set_yydebug (get_program_option ("debug-parser"));

  new_input (main_input_name_, sources_);

  scm_module_define (scm_car (scopes_),
                     ly_symbol2scm ("input-file-name"),
                     ly_string2scm (main_input_name_));
}

void
Lily_lexer::new_input (const string &str, string d, Sources *ss)
{
  Includable_lexer::new_input (str, d, ss);
}

void
Lily_lexer::new_input (const string &str, Sources *ss)
{
  if (is_main_input_ && be_safe_global)
    {
      LexerError (_ ("include files are not allowed in safe mode").c_str ());
      return;
    }

  Includable_lexer::new_input (str, ss);
}

// PATH is either a single symbol (or string) or a list of symbols
// giving the path to a nested property.  A symbol is treated the same
// as a list of length 1.
void
Lily_lexer::set_identifier (SCM path, SCM val)
{
  SCM sym = path;
  if (scm_is_string (path))
    sym = scm_string_to_symbol (path);
  else if (scm_is_pair (path))
    {
      sym = scm_car (path);
      path = scm_cdr (path);
    }

  if (scm_is_symbol (sym))
    {
      if (lookup_keyword (ly_symbol2string (sym)) >= 0)
        {
          string symstr = ly_symbol2string (sym);
          warning (_f ("identifier name is a keyword: `%s'", symstr.c_str ()));
        }

      SCM mod = scm_car (scopes_);

      if (scm_is_pair (path))
        {
          SCM prev = ly_module_lookup (mod, sym);
          if (scm_is_true (prev))
            val = nested_property_alist (scm_variable_ref (prev), path, val);
          else
            val = nested_create_alist (path, val);
        }
      scm_module_define (mod, sym, val);
    }
  else
    programming_error ("identifier is not a symbol");
}

void
Lily_lexer::LexerError (char const *s)
{
  if (include_stack_.empty ())
    non_fatal_error (s, _f ("%s:EOF", s));
  else
    {
      error_level_ |= 1;
      Input spot (*lexloc_);
      spot.non_fatal_error (s);
    }
}

void
Lily_lexer::LexerWarning (char const *s)
{
  if (include_stack_.empty ())
    warning (s, _f ("%s:EOF", s));
  else
    {
      Input spot (*lexloc_);
      spot.warning (s);
    }
}

char
Lily_lexer::escaped_char (char c) const
{
  switch (c)
    {
    case 'n':
      return '\n';
    case 't':
      return '\t';
    case '\'':
    case '\"':
    case '\\':
      return c;
    }
  return 0;
}

Input
Lily_lexer::here_input () const
{
  return Input (*lexloc_);
}

Input const &
Lily_lexer::override_input (Input const &in) const
{
  return override_input_.get_source_file ()
    ? override_input_ : in;
}

void
Lily_lexer::prepare_for_next_token ()
{
  last_input_ = here_input ();
}

/**
   Since we don't create the buffer state from the bytes directly, we
   don't know about the location of the lexer. Add this as a
   YY_USER_ACTION */
void
Lily_lexer::add_lexed_char (int count)
{
  char const *start = here_str0 ();
  lexloc_->set (get_source_file (),
                start, start + count);
  char_count_stack_.back () += count;
}


const char Lily_lexer::type_p_name_[] = "ly:lily-lexer?";

SCM
Lily_lexer::mark_smob () const
{
  ASSERT_LIVE_IS_ALLOWED (self_scm ());

  scm_gc_mark (chordmodifier_tab_);
  if (parser_)
    scm_gc_mark (parser_->self_scm ());
  scm_gc_mark (pitchname_tab_stack_);
  scm_gc_mark (start_module_);
  scm_gc_mark (extra_tokens_);
  return scopes_;
}

int
Lily_lexer::print_smob (SCM port, scm_print_state *) const
{
  scm_puts ("#<Lily_lexer ", port);
  scm_display (scopes_, port);
  scm_puts (" >", port);
  return 1;
}

bool
Lily_lexer::is_clean () const
{
  return include_stack_.empty ();
}
