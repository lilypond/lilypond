/*
  my-lily-lexer.cc -- implement My_lily_lexer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
#include "ly-modules.hh"


static Keyword_ent the_key_tab[]={
  {"alias", ALIAS},
  {"apply", APPLY},
  {"arpeggio", ARPEGGIO },
  {"autochange", AUTOCHANGE},
  {"spanrequest", SPANREQUEST},
  {"commandspanrequest", COMMANDSPANREQUEST},  
  {"simultaneous", SIMULTANEOUS},
  {"sequential", SEQUENTIAL},
  {"accepts", ACCEPTS},
  {"alternative", ALTERNATIVE},
  {"bar", BAR},
  {"breathe", BREATHE},
  {"char", CHAR_T},
  {"chordmodifiers", CHORDMODIFIERS},
  {"chords", CHORDS},
  {"clef", CLEF},
  {"cm", CM_T},
  {"consists", CONSISTS},
  {"consistsend", CONSISTSEND},
  {"context", CONTEXT},
  {"default", DEFAULT},
  {"denies", DENIES},
  {"duration", DURATION},
  {"grobdescriptions", GROBDESCRIPTIONS},
  {"figures",FIGURES},
  {"grace", GRACE},
  {"glissando", GLISSANDO},
  {"header", HEADER},
  {"in", IN_T},
  {"lyrics", LYRICS},
  {"key", KEY},
  {"mark", MARK},
  {"once", ONCE},
  {"pitch", PITCH},
  {"time", TIME_T},
  {"times", TIMES},
  {"midi", MIDI},
  {"mm", MM_T},
  {"name", NAME},
  {"pitchnames", PITCHNAMES},
  {"notes", NOTES},
  {"outputproperty", OUTPUTPROPERTY},
  {"override", OVERRIDE},
  {"set", SET},
  {"rest", REST},
  {"revert", REVERT},
  {"partial", PARTIAL},
  {"paper", PAPER},
  {"penalty", PENALTY},
  {"property", PROPERTY},
  {"pt", PT_T},
  {"relative", RELATIVE},
  {"remove", REMOVE},
  {"repeat", REPEAT},
  {"addlyrics", ADDLYRICS},
  {"partcombine", PARTCOMBINE},
  {"score", SCORE},
  {"script", SCRIPT},
  {"stylesheet", STYLESHEET},
  {"skip", SKIP},
  {"tempo", TEMPO},
  {"translator", TRANSLATOR},
  {"transpose", TRANSPOSE},
  {"type", TYPE},
  {"unset", UNSET},
  {0,0}
};


My_lily_lexer::My_lily_lexer ()
{
  keytable_ = new Keyword_table (the_key_tab);
  scopes_ = SCM_EOL;
  
  add_scope(ly_make_anonymous_module());
  errorlevel_ =0; 

  main_input_b_ = false;
}

void
My_lily_lexer::add_scope (SCM module)
{
  ly_reexport_module (scm_current_module());
  scm_set_current_module (module);
  for (SCM s = scopes_; gh_pair_p (s); s = gh_cdr (s))
    {
      /*
	UGH. how to do this more neatly? 
      */      
      SCM expr = scm_list_n (ly_symbol2scm ("module-use!"),
			     module, scm_list_n (ly_symbol2scm ("module-public-interface"),
						 gh_car (s), SCM_UNDEFINED),
			     SCM_UNDEFINED);
      
      scm_primitive_eval(expr);
    }
  
  scopes_ = scm_cons (module, scopes_);
}

SCM
My_lily_lexer::remove_scope ()
{
  SCM sc = gh_car (scopes_);
  scopes_ = gh_cdr (scopes_);
  scm_set_current_module (gh_car (scopes_));

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
  for (SCM s = scopes_; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM var = ly_module_lookup (gh_car (s), sym);
      if (var != SCM_BOOL_F)
	return scm_variable_ref(var);
    }

  return SCM_UNSPECIFIED;
}

void
My_lily_lexer::start_main_input ()
{  
  new_input (main_input_string_, &global_input_file->sources_);
  allow_includes_b_ = allow_includes_b_ &&  ! (safe_global_b);
}

void
My_lily_lexer::set_identifier (SCM name, SCM s)
{
  assert (gh_string_p (name));
  
  if (lookup_keyword (ly_scm2string (name)) >= 0)
    {
      size_t sz;
      char * str = gh_scm2newstr (name, &sz) ;
      warning (_f ("Identifier name is a keyword: `%s'", str));
      free  (str);
    }

  SCM sym = scm_string_to_symbol (name);
  SCM mod = gh_car (scopes_);

  scm_module_define (mod, sym, s);
}

My_lily_lexer::~My_lily_lexer ()
{
  delete keytable_;
}



void
My_lily_lexer::LexerError (char const *s)
{
  if (include_stack_.empty ())
    {
      progress_indication (_f ("error at EOF: %s", s)+ String ("\n"));
    }
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
  last_input_ = here_input();
}
