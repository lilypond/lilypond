/*
  midi-def.cc -- implement Midi_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

*/
#include <math.h>
#include "misc.hh"
#include "midi-def.hh"
#include "input-translator.hh"
#include "performer-group-performer.hh"
#include "assoc-iter.hh"

#include "debug.hh"

// classes, alphasorted
//     statics
//     constructors
//     destructor
//     routines, alphasorted

// statics Midi_def
// ugh

int Midi_def::den_i_s = 4;
int Midi_def::num_i_s = 4;

Midi_def::Midi_def()
{
  outfile_str_ = ""; 
  itrans_p_ = 0;
  // ugh
  set_tempo (Moment (1, 4), 60 );
}

Midi_def::Midi_def (Midi_def const& s)
{
  whole_seconds_f_ = s.whole_seconds_f_;
  itrans_p_ = s.itrans_p_ ? new Input_translator (*s.itrans_p_) : 0;
  outfile_str_ = s.outfile_str_;
}

Midi_def::~Midi_def()
{
  delete itrans_p_;
}

Real
Midi_def::duration_to_seconds_f (Moment mom)
{
  if ( !mom)
	return 0;
  
  return Moment (whole_seconds_f_) * mom;
}

Global_translator*
Midi_def::get_global_translator_p() const
{
  return  itrans_p_->get_group_performer_p()->global_l ();
}

int
Midi_def::get_tempo_i (Moment moment)
{
  return Moment (whole_seconds_f_) * Moment (60 ) * moment;
}

void
Midi_def::print() const
{
#ifndef NPRINT
  DOUT << "Midi {";
  DOUT << "4/min: " << Real (60) / ( whole_seconds_f_ * 4 );
  DOUT << "out: " << outfile_str_;
  DOUT << "}\n";
#endif
}

void
Midi_def::set (Input_translator* itrans_p)
{
  delete itrans_p_;
  itrans_p_ = itrans_p;
}

void
Midi_def::set_tempo (Moment moment, int count_per_minute_i)
{
  whole_seconds_f_ = Moment (count_per_minute_i) / Moment (60 ) / moment;
}

