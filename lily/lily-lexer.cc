/*
  lily-lexer.cc -- implement Lily_lexer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <ctype.h>
#include <sstream>

#include "lily-proto.hh"
#include "scm-hash.hh"
#include "interval.hh"
#include "lily-guile.hh"
#include "parser.hh"
#include "keyword.hh"
#include "lily-lexer.hh"
#include "warn.hh"
#include "source-file.hh"
#include "main.hh"
#include "input.hh"
#include "moment.hh"
#include "ly-module.hh"


static Keyword_ent the_key_tab[] = {
  {"accepts", ACCEPTS},
  {"addquote", ADDQUOTE},
  {"addlyrics", ADDLYRICS},
  {"alias", ALIAS},
  {"alternative", ALTERNATIVE},
  {"bar", BAR},
  {"book", BOOK},
  {"bookpaper", BOOKPAPER},
  {"change", CHANGE},
  {"chords", CHORDS},
  {"chordmode", CHORDMODE},
  {"clef", CLEF},
  {"consists", CONSISTS},
  {"context", CONTEXT},
  {"default", DEFAULT},
  {"denies", DENIES},
  {"drummode", DRUMMODE},
  {"drums", DRUMS},
  {"description", DESCRIPTION},
  {"figures",FIGURES},
  {"figuremode",FIGUREMODE},
  {"grobdescriptions", GROBDESCRIPTIONS},
  {"header", HEADER},
  {"key", KEY},
  {"lyricmode", LYRICMODE},
  {"lyricsto", LYRICSTO},
  {"lyrics", LYRICS},
  {"mark", MARK},
  {"markup", MARKUP},
  {"midi", MIDI},
  {"name", NAME},
  {"new", NEWCONTEXT},
  {"notemode", NOTEMODE},
  {"octave", OCTAVE},
  {"once", ONCE},
  {"override", OVERRIDE},
  {"paper", PAPER},
  {"partial", PARTIAL},
  {"quote", QUOTE},
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
  {"tag", TAG},
  {"tempo", TEMPO},
  {"time", TIME_T},
  {"times", TIMES},
  {"transpose", TRANSPOSE},
  {"transposition", TRANSPOSITION},
  {"type", TYPE},
  {"unset", UNSET},
  {"with", WITH},
  {0, 0}
};


Lily_lexer::Lily_lexer (Sources *sources)
{
  keytable_ = new Keyword_table (the_key_tab);
  encoding_ = SCM_EOL;
  chordmodifier_tab_ = SCM_EOL;
  pitchname_tab_stack_ = SCM_EOL; 
  sources_ = sources;
  scopes_ = SCM_EOL;
  error_level_ = 0; 
  main_input_b_ = false;

  smobify_self ();
  
  add_scope (ly_make_anonymous_module (false));
  push_note_state (scm_c_make_hash_table (0));
  chordmodifier_tab_ = scm_make_vector (scm_int2num (1), SCM_EOL);
}

Lily_lexer::Lily_lexer (Lily_lexer const &src)
  : Includable_lexer ()
{
  keytable_ = (src.keytable_) ? new Keyword_table (*src.keytable_) : 0;
  encoding_ = src.encoding_;
  chordmodifier_tab_ = src.chordmodifier_tab_;
  pitchname_tab_stack_ = src.pitchname_tab_stack_;
  sources_ = src.sources_;
  
  error_level_ = src.error_level_; 
  main_input_b_ = src.main_input_b_;

  scopes_ = SCM_EOL;
  
  smobify_self ();
  
  SCM scopes = SCM_EOL;
  SCM *tail = &scopes;
  for (SCM s = src.scopes_; ly_c_pair_p (s); s = ly_cdr (s))
    {
      SCM newmod = ly_make_anonymous_module (false);
      ly_import_module (newmod, ly_car (s));
      *tail = scm_cons (newmod, SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
  
  scopes_ =  scopes;
  push_note_state (scm_c_make_hash_table (0));
}

Lily_lexer::~Lily_lexer ()
{
  delete keytable_;
}

SCM
Lily_lexer::encoding () const
{
  return encoding_ ;
}


void
Lily_lexer::add_scope (SCM module)
{
  ly_reexport_module (scm_current_module ());
  scm_set_current_module (module);
  for (SCM s = scopes_; ly_c_pair_p (s); s = ly_cdr (s))
    {
      ly_use_module (module, ly_car (s));
    }
  scopes_ = scm_cons (module, scopes_);
}

SCM
Lily_lexer::remove_scope ()
{
  SCM sc = ly_car (scopes_);
  scopes_ = ly_cdr (scopes_);
  scm_set_current_module (ly_car (scopes_));

  return sc;
}


int
Lily_lexer::lookup_keyword (String s)
{
  return keytable_->lookup (s.to_str0 ());
}

SCM
Lily_lexer::lookup_identifier_symbol (SCM sym)
{
  for (SCM s = scopes_; ly_c_pair_p (s); s = ly_cdr (s))
    {
      SCM var = ly_module_lookup (ly_car (s), sym);
      if (var != SCM_BOOL_F)
	return scm_variable_ref (var);
    }

  return SCM_UNDEFINED;
}

SCM
Lily_lexer::lookup_identifier (String name)
{
  return lookup_identifier_symbol (ly_symbol2scm (name.to_str0 ()));
}

void
Lily_lexer::start_main_input ()
{
  // yy_flex_debug = 1;
  new_input (main_input_name_, sources_);
  
  /* Do not allow \include in --safe-mode */
  allow_includes_b_ = allow_includes_b_ && !safe_global_b;

  scm_module_define (ly_car (scopes_),
		     ly_symbol2scm ("input-file-name"),
		     scm_makfrom0str (main_input_name_.to_str0 ()));
}

void
Lily_lexer::set_identifier (SCM name, SCM s)
{
  SCM sym = name;
  if (scm_is_string (name))
    sym =  scm_string_to_symbol (name);
  
  if (scm_is_symbol (sym))
    {
      if (lookup_keyword (ly_symbol2string (sym)) >= 0)
	{
	  String symstr = ly_symbol2string (sym); 
	  warning (_f ("Identifier name is a keyword: `%s'", symstr.to_str0()));
	}

      SCM mod = ly_car (scopes_);

      scm_module_define (mod, sym, s);
    }
  else
    {
      programming_error ("Identifier is not a symbol.");
    }
}

void
Lily_lexer::LexerError (char const *s)
{
  if (include_stack_.is_empty ())
    progress_indication (_f ("error at EOF: %s", s) + String ("\n"));
  else
    {
      error_level_ |= 1;
      Input spot (get_source_file (), here_str0 ());
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
  Source_file * f= get_source_file ();
  return Input (f, (char*)here_str0 ());
}

void
Lily_lexer::prepare_for_next_token ()
{
  last_input_ = here_input ();
}

void
Lily_lexer::set_encoding (String s)
{
  if (s.length ())
    encoding_ = ly_symbol2scm (s.to_str0 ());
  else
    encoding_ = SCM_EOL;
}

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Lily_lexer);
IMPLEMENT_TYPE_P (Lily_lexer, "ly:lily-lexer?");
IMPLEMENT_DEFAULT_EQUAL_P (Lily_lexer);

SCM
Lily_lexer::mark_smob (SCM s)
{
  Lily_lexer *lexer = (Lily_lexer*) ly_cdr (s);

  scm_gc_mark (lexer->chordmodifier_tab_);
  scm_gc_mark (lexer->pitchname_tab_stack_);
  scm_gc_mark (lexer->scopes_);
  return lexer->encoding_;
}

int
Lily_lexer::print_smob (SCM, SCM port, scm_print_state*)
{
  scm_puts ("#<Lily_lexer ", port);
  scm_puts (" >", port);
  return 1;
}
