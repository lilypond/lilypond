/*
  my-lily-lexer.cc -- implement My_lily_lexer

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <strstream.h>
#include <ctype.h>
#include "notename-table.hh"
#include "interval.hh"
#include "identifier.hh"
#include "parser.hh"
#include "keyword.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "source-file.hh"
#include "main.hh"
#include "scope.hh"
#include "input.hh"

static Keyword_ent the_key_tab[]={
  {"spanrequest", SPANREQUEST},
  {"simultaneous", SIMULTANEOUS},
  {"sequential", SEQUENTIAL},
  {"accepts", ACCEPTS},
  {"alternative", ALTERNATIVE},
  {"bar", BAR},
  {"breathe", BREATHE},
  {"cadenza", CADENZA},
  {"chordmodifiers", CHORDMODIFIERS},
  {"chords", CHORDS},
  {"clef", CLEF},
  {"cm", CM_T},
  {"consists", CONSISTS},
  {"consistsend", CONSISTSEND},
  {"context", CONTEXT},
  {"duration", DURATION},
  {"font", FONT},
  {"grace", GRACE},
  {"header", HEADER},
  {"in", IN_T},
  {"lyrics", LYRICS},
  {"key", KEY},
  {"keysignature", KEYSIGNATURE},
  {"mark", MARK},
  {"musicalpitch", MUSICAL_PITCH},
  {"time", TIME_T},
  {"times", TIMES},
  {"midi", MIDI},
  {"mm", MM_T},
  {"name", NAME},
  {"notenames", NOTENAMES},
  {"notes", NOTES},
  {"partial", PARTIAL},
  {"paper", PAPER},
  {"penalty", PENALTY},
  {"property", PROPERTY},
  {"pt", PT_T},
  {"relative", RELATIVE},
  {"remove", REMOVE},
  {"repeat", REPEAT},
  {"repetitions", REPETITIONS},
  {"addlyrics", ADDLYRICS},
  {"scm", SCM_T},
  {"scmfile", SCMFILE},
  {"score", SCORE},
  {"script", SCRIPT},
  {"shape", SHAPE},
  {"skip", SKIP},
  {"textscript", TEXTSCRIPT},
  {"tempo", TEMPO},
  {"translator", TRANSLATOR},
  {"transpose", TRANSPOSE},
  {"type", TYPE},
  {"version", VERSION},
  {0,0}
};

My_lily_lexer::My_lily_lexer()
{
  keytable_p_ = new Keyword_table (the_key_tab);
  toplevel_scope_p_ = new Scope;
  scope_l_arr_.push (toplevel_scope_p_);
  errorlevel_i_ = 0;
  note_tab_p_ = new Notename_table;
  chordmodifier_tab_p_ = new Notename_table;
  main_input_b_ = false;
}

int
My_lily_lexer::lookup_keyword (String s)
{
  return keytable_p_->lookup (s.ch_C ());
}

Identifier*
My_lily_lexer::lookup_identifier (String s)
{
  SCM sym = ly_symbol (s.ch_C());
  
  for (int i = scope_l_arr_.size (); i--; )
    if (scope_l_arr_[i]->elem_b (sym))
      return scope_l_arr_[i]->elem(sym);
  return 0;
}

void
My_lily_lexer::start_main_input ()
{  
  if (!lily_monitor->silent_b ("InitDeclarations") && check_debug)
    print_declarations (true);
  if (!lily_monitor->silent_b ("InitLexer") && check_debug)
    set_debug (1);


  new_input (main_input_str_, source_global_l);
  if (safe_global_b)
    allow_includes_b_ = false;
  
  print_declarations(true);
}

void
My_lily_lexer::set_identifier (String name_str, Identifier* i, bool )
{
  Identifier *old =0;
  if (scope_l_arr_.top ()->elem_b (name_str))
    old = scope_l_arr_.top ()->elem(name_str);
 
   
  if  (old)
    {
#if 0
      if (unique_b)
	old->warning(_f ("redeclaration of `\\%s'", name_str));
#endif
      delete old;
    }
  if (lookup_keyword (name_str) >= 0)
    {
      warning (  _f ("Identifier name is a keyword: `%s'", name_str));
    }
  
  scope_l_arr_.top ()->elem (name_str) = i;
}

My_lily_lexer::~My_lily_lexer()
{
  delete chordmodifier_tab_p_;
  delete keytable_p_;
  delete toplevel_scope_p_ ;
  delete note_tab_p_;
}

void
My_lily_lexer::print_declarations (bool ) const
{
  for (int i=scope_l_arr_.size (); i--; )
    {
      DOUT << "Scope no. " << i << '\n';
      scope_l_arr_[i]->print ();
    }
}

void
My_lily_lexer::LexerError (char const *s)
{
  if (include_stack_.empty())
    {
      *mlog << _f ("error at EOF: %s", s) << endl;
    }
  else
    {
      errorlevel_i_ |= 1;
      Input spot (source_file_l(),here_ch_C());
      spot.error (s);
    }
}

Musical_pitch
My_lily_lexer::lookup_notename (String s)
{
  return (*note_tab_p_)[s];
}

Musical_pitch
My_lily_lexer::lookup_chordmodifier (String s)
{
  return (*chordmodifier_tab_p_)[s];
}

bool
My_lily_lexer::notename_b (String s) const
{
  return note_tab_p_->elem_b (s);
}

void
My_lily_lexer::set_notename_table (Notename_table *p)
{
  delete note_tab_p_;
  note_tab_p_ = p;
}

bool
My_lily_lexer::chordmodifier_b (String s) const
{
  return chordmodifier_tab_p_->elem_b (s);
}

void
My_lily_lexer::set_chordmodifier_table (Notename_table *p)
{
  delete chordmodifier_tab_p_;
  chordmodifier_tab_p_ = p;
}

char
My_lily_lexer::escaped_char(char c) const
{
  switch(c)
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
  Source_file * f_l= source_file_l ();
  return Input (f_l, (char*)here_ch_C ());
}
