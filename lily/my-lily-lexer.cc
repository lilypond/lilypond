/*
  my-lily-lexer.cc -- implement My_lily_lexer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <ctype.h>
#include <sstream>

#include "lily-proto.hh"
#include "scm-hash.hh"
#include "interval.hh"
#include "input-file-results.hh"
#include "lily-guile.hh"
#include "parser.hh"
#include "keyword.hh"
#include "my-lily-lexer.hh"
#include "warn.hh"
#include "source-file.hh"
#include "main.hh"
#include "input.hh"
#include "moment.hh"
#include "ly-module.hh"


static Keyword_ent the_key_tab[] = {
  {"acciaccatura", ACCIACCATURA},
  {"accepts", ACCEPTS},
  {"addlyrics", ADDLYRICS},
  {"addquote", ADDQUOTE},
  {"alias", ALIAS},
  {"alternative", ALTERNATIVE},
  {"apply", APPLY},
  {"applycontext", APPLYCONTEXT},
  {"applyoutput", APPLYOUTPUT},
  {"appoggiatura", APPOGGIATURA},
  {"autochange", AUTOCHANGE},
  {"bar", BAR},
  {"breathe", BREATHE},
  {"change", CHANGE},
  {"chords", CHORDS},
  {"clef", CLEF},
  {"consists", CONSISTS},
  {"consistsend", CONSISTSEND},
  {"context", CONTEXT},
  {"default", DEFAULT},
  {"denies", DENIES},
  {"drums", DRUMS},
  {"description", DESCRIPTION},
  {"figures",FIGURES},
  {"grace", GRACE},
  {"grobdescriptions", GROBDESCRIPTIONS},
  {"header", HEADER},
  {"key", KEY},
  {"lyrics", LYRICS},
  {"lyricsto", NEWADDLYRICS},
  {"mark", MARK},
  {"markup", MARKUP},
  {"midi", MIDI},
  {"name", NAME},
  {"new", NEWCONTEXT},
  {"notes", NOTES},
  {"octave", OCTAVE},
  {"once", ONCE},
  {"override", OVERRIDE},
  {"paper", PAPER},
  {"partcombine", PARTCOMBINE},
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


My_lily_lexer::My_lily_lexer ()
{
  keytable_ = new Keyword_table (the_key_tab);
  encoding_ = SCM_EOL;
  chordmodifier_tab_ = scm_make_vector (scm_int2num (1), SCM_EOL);
  pitchname_tab_stack_ = SCM_EOL; 
  
  scopes_ = SCM_EOL;
  
  add_scope (ly_make_anonymous_module ());
  errorlevel_ = 0; 

  main_input_b_ = false;
}

SCM
My_lily_lexer::encoding () const
{
  return encoding_ ;
}

void
My_lily_lexer::add_scope (SCM module)
{
  ly_reexport_module (scm_current_module ());
  scm_set_current_module (module);
  for (SCM s = scopes_; is_pair (s); s = ly_cdr (s))
    {
      /* UGH. how to do this more neatly? */      
      SCM expr
	= scm_list_3 (ly_symbol2scm ("module-use!"),
		      module,
		      scm_list_2 (ly_symbol2scm ("module-public-interface"),
				  ly_car (s)));
      scm_primitive_eval (expr);
    }
  scopes_ = scm_cons (module, scopes_);
}

SCM
My_lily_lexer::remove_scope ()
{
  SCM sc = ly_car (scopes_);
  scopes_ = ly_cdr (scopes_);
  scm_set_current_module (ly_car (scopes_));

  return sc;
}


int
My_lily_lexer::lookup_keyword (String s)
{
  return keytable_->lookup (s.to_str0 ());
}

SCM
My_lily_lexer::lookup_identifier (String s)
{
  SCM sym = ly_symbol2scm (s.to_str0());
  for (SCM s = scopes_; is_pair (s); s = ly_cdr (s))
    {
      SCM var = ly_module_lookup (ly_car (s), sym);
      if (var != SCM_BOOL_F)
	return scm_variable_ref (var);
    }

  return SCM_UNDEFINED;
}

void
My_lily_lexer::start_main_input ()
{
  // yy_flex_debug = 1;

  new_input (main_input_name_, &global_input_file->sources_);
  /* Do not allow \include in --safe-mode */
  allow_includes_b_ = allow_includes_b_ && ! safe_global_b;

  scm_module_define (ly_car (scopes_),
		     ly_symbol2scm ("input-file-name"),
		     scm_makfrom0str (main_input_name_.to_str0 ()));
}

void
My_lily_lexer::set_identifier (SCM name, SCM s)
{
  assert (is_string (name));
  
  if (lookup_keyword (ly_scm2string (name)) >= 0)
    {
      warning (_f ("Identifier name is a keyword: `%s'", SCM_STRING_CHARS (name)));
    }

  SCM sym = scm_string_to_symbol (name);
  SCM mod = ly_car (scopes_);

  scm_module_define (mod, sym, s);
}

My_lily_lexer::~My_lily_lexer ()
{
  delete keytable_;
}



void
My_lily_lexer::LexerError (char const *s)
{
  if (include_stack_.is_empty ())
    progress_indication (_f ("error at EOF: %s", s) + String ("\n"));
  else
    {
      errorlevel_ |= 1;
      Input spot (get_source_file (), here_str0 ());
      spot.error (s);
    }
}

char
My_lily_lexer::escaped_char (char c) const
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
My_lily_lexer::here_input () const
{
  Source_file * f= get_source_file ();
  return Input (f, (char*)here_str0 ());
}

void
My_lily_lexer::prepare_for_next_token ()
{
  last_input_ = here_input ();
}

void
My_lily_lexer::set_encoding (String s)
{
  encoding_ = ly_symbol2scm (s.to_str0 ());
}
