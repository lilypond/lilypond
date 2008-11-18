/*
  lily-lexer.cc -- implement Lily_lexer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "lily-lexer.hh"

#include <cctype>
#include <sstream>
using namespace std;

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

static Keyword_ent the_key_tab[]
= {
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
  {"figuremode", FIGUREMODE},
  {"figures", FIGURES},
  {"grobdescriptions", GROBDESCRIPTIONS},
  {"header", HEADER},
  {"key", KEY},
  {"layout", LAYOUT},
  {"lyricmode", LYRICMODE},
  {"lyrics", LYRICS},
  {"lyricsto", LYRICSTO},
  {"mark", MARK},
  {"markup", MARKUP},
  {"markuplines", MARKUPLINES},
  {"midi", MIDI},
  {"name", NAME},
  {"new", NEWCONTEXT},
  {"notemode", NOTEMODE},
  {"objectid", OBJECTID},
  {"once", ONCE},
  {"override", OVERRIDE},
  {"paper", PAPER},
  {"partial", PARTIAL},
  {"relative", RELATIVE},
  {"remove", REMOVE},
  {"repeat", REPEAT},
  {"rest", REST},
  {"revert", REVERT},
  {"score", SCORE},
  {"sequential", SEQUENTIAL},
  {"set", SET},
  {"simultaneous", SIMULTANEOUS},
  {"skip", SKIP},
  {"tempo", TEMPO},
  {"time", TIME_T},
  {"times", TIMES},
  {"transpose", TRANSPOSE},
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
  start_module_ = SCM_EOL;
  smobify_self ();

  add_scope (ly_make_anonymous_module (false));
  push_note_state (scm_c_make_hash_table (0));
  chordmodifier_tab_ = scm_make_vector (scm_from_int (1), SCM_EOL);
}

Lily_lexer::Lily_lexer (Lily_lexer const &src, Lily_parser *parser)
  : Includable_lexer ()
{
  parser_ = parser; 
  keytable_ = (src.keytable_) ? new Keyword_table (*src.keytable_) : 0;
  chordmodifier_tab_ = src.chordmodifier_tab_;
  pitchname_tab_stack_ = src.pitchname_tab_stack_;
  sources_ = src.sources_;
  start_module_ = SCM_EOL;

  error_level_ = src.error_level_;
  is_main_input_ = src.is_main_input_;

  scopes_ = SCM_EOL;

  smobify_self ();

  SCM scopes = SCM_EOL;
  SCM *tail = &scopes;
  for (SCM s = src.scopes_; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM newmod = ly_make_anonymous_module (false);
      ly_module_copy (newmod, scm_car (s));
      *tail = scm_cons (newmod, SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }

  scopes_ = scopes;
  push_note_state (scm_c_make_hash_table (0));
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
Lily_lexer::lookup_keyword (string s)
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
      *tail = scm_acons (scm_from_locale_string (keytable_->table_[i].name_),
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
      if (var != SCM_BOOL_F)
	return scm_variable_ref (var);
    }

  return SCM_UNDEFINED;
}

SCM
Lily_lexer::lookup_identifier (string name)
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
Lily_lexer::new_input (string str, string d, Sources *ss)
{
  Includable_lexer::new_input (str, d, ss);
}

void
Lily_lexer::new_input (string str, Sources *ss)
{
  if (is_main_input_ && be_safe_global)
    {
      LexerError (_ ("include files are not allowed in safe mode").c_str ());
      return;
    }

  Includable_lexer::new_input (str, ss);
}

void
Lily_lexer::set_identifier (SCM name, SCM s)
{
  SCM sym = name;
  if (scm_is_string (name))
    sym = scm_string_to_symbol (name);

  if (scm_is_symbol (sym))
    {
      if (lookup_keyword (ly_symbol2string (sym)) >= 0)
	{
	  string symstr = ly_symbol2string (sym);
	  warning (_f ("identifier name is a keyword: `%s'", symstr.c_str ()));
	}

      SCM mod = scm_car (scopes_);

      scm_module_define (mod, sym, s);
    }
  else
    programming_error ("identifier is not a symbol");
}

void
Lily_lexer::LexerError (char const *s)
{
  if (include_stack_.empty ())
    message (_f ("error at EOF: %s", s) + "\n");
  else
    {
      error_level_ |= 1;
      Input spot (*lexloc);
      spot.error (s);
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
  return Input (*lexloc);
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
  lexloc->set (get_source_file (),
	       start, start + count);
  char_count_stack_.back () += count;
}

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Lily_lexer);
IMPLEMENT_TYPE_P (Lily_lexer, "ly:lily-lexer?");
IMPLEMENT_DEFAULT_EQUAL_P (Lily_lexer);

SCM
Lily_lexer::mark_smob (SCM s)
{
  ASSERT_LIVE_IS_ALLOWED ();
  
  Lily_lexer *lexer = (Lily_lexer *) SCM_CELL_WORD_1 (s);

  scm_gc_mark (lexer->chordmodifier_tab_);
  if (lexer->parser_)
    scm_gc_mark (lexer->parser_->self_scm ());
  scm_gc_mark (lexer->pitchname_tab_stack_);
  scm_gc_mark (lexer->start_module_);
  return lexer->scopes_;
}

int
Lily_lexer::print_smob (SCM s, SCM port, scm_print_state*)
{
  Lily_lexer *lexer = Lily_lexer::unsmob (s);

  scm_puts ("#<Lily_lexer ", port);
  scm_display (lexer->scopes_, port);
  scm_puts (" >", port);
  return 1;
}
