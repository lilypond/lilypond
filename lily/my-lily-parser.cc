/*
  my-lily-parser.cc -- implement My_lily_parser

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
       Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "my-lily-parser.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "main.hh"
#include "music-list.hh"
#include "musical-request.hh"
#include "command-request.hh"
#include "lily-guile.hh"
#include "parser.hh"
#include "scope.hh"
#include "file-results.hh"
#include "midi-def.hh"
#include "paper-def.hh"
#include "identifier.hh"
#include "chord.hh"

My_lily_parser::My_lily_parser (Sources * source_l)
{
  source_l_ = source_l;
  lexer_p_ = 0;
  default_duration_.durlog_i_ = 2;
  error_level_i_ = 0;

  fatal_error_i_ = 0;
  default_header_p_ =0;
}

My_lily_parser::~My_lily_parser()
{
  delete lexer_p_;
  delete default_header_p_;
}

void
My_lily_parser::set_version_check (bool )
{
}

void
My_lily_parser::parse_file (String init, String s)
{
  lexer_p_ = new My_lily_lexer;

  lexer_p_->main_input_str_ = s;

  progress_indication (_("Parsing..."));

  set_yydebug (flower_dstream &&!flower_dstream->silent_b ("Parser"));
  lexer_p_->new_input (init, source_l_);
  do_yyparse ();

  if (!define_spot_array_.empty())
    {
      warning (_ ("Braces don't match"));
      error_level_i_ = 1;
    }

  inclusion_global_array = lexer_p_->filename_str_arr_;

  error_level_i_ = error_level_i_ | lexer_p_->errorlevel_i_; // ugh naming.
}

void
My_lily_parser::remember_spot()
{
  define_spot_array_.push (here_input());
}

char const *
My_lily_parser::here_ch_C() const
{
  return lexer_p_->here_ch_C();
}

void
My_lily_parser::parser_error (String s)
{
  here_input().error (s);
  if (fatal_error_i_)
    exit (fatal_error_i_);
  error_level_i_ = 1;
  exit_status_i_ = 1;
}

void
My_lily_parser::set_last_duration (Duration const *d)
{
  default_duration_ = *d;
}

// junk me
Simultaneous_music *
My_lily_parser::get_chord (Musical_pitch tonic,
			   Array<Musical_pitch>* add_arr_p,
			   Array<Musical_pitch>* sub_arr_p,
			   Musical_pitch* inversion_p,
			   Musical_pitch* bass_p,
			   Duration d)
{
  Simultaneous_music*v = new Request_chord;
  v->set_spot (here_input ());

  /*
    UARGAUGRAGRUAUGRUINAGRAUGIRNA

    ugh
   */
  Chord chord = to_chord (tonic, add_arr_p, sub_arr_p, inversion_p, bass_p);
  inversion_p = 0;
  bass_p = 0;

  Tonic_req* t = new Tonic_req;
  t->pitch_ = tonic;
  v->add_music (t);

  //urg
  if (chord.inversion_b_
      && Chord::find_notename_i (&chord.pitch_arr_, chord.inversion_pitch_) > 0)
    {
      Inversion_req* i = new Inversion_req;
      i->pitch_ = chord.inversion_pitch_;
      v->add_music (i);
    }

  if (chord.bass_b_)
    {
      Bass_req* b = new Bass_req;
      b->pitch_ = chord.bass_pitch_;
      v->add_music (b);
    }

  Array<Musical_pitch> pitch_arr = chord.to_pitch_arr ();
  for (int i = 0; i < pitch_arr.size (); i++)
    {
      Musical_pitch p = pitch_arr[i];
      Note_req* n = new Note_req;
      n->pitch_ = p;
      n->duration_ = d;
      v->add_music (n);
    }

  return v;
}



Input
My_lily_parser::pop_spot()
{
  return define_spot_array_.pop();
}

Input
My_lily_parser::here_input() const
{
  return  lexer_p_->here_input ();
}



