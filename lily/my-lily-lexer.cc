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
  {"dynamicscript", DYNAMICSCRIPT},
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
  toplevel_variable_tab_ = new Scheme_hash_table ;
  scopes_.push (toplevel_variable_tab_);
  
  errorlevel_ = 0;
  main_input_b_ = false;
}

int
My_lily_lexer::lookup_keyword (String s)
{
  return keytable_->lookup (s.to_str0 ());
}

SCM
My_lily_lexer::lookup_identifier (String s)
{
  SCM sym = ly_symbol2scm (s.to_str0 ());
  
  for (int i = scopes_.size (); i--;)
    {
      SCM val = SCM_UNSPECIFIED;
      if (scopes_[i]->try_retrieve (sym, &val))
	return val;
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
  
  scopes_.top ()->set (scm_string_to_symbol (name), s);
}

My_lily_lexer::~My_lily_lexer ()
{
  delete keytable_;
  scm_gc_unprotect_object (toplevel_variable_tab_->self_scm ());
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
      Input spot (get_source_file (),here_str0 ());
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
