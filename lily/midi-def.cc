/*
  midi-def.cc -- implement Midi_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

*/
#include <math.h>
#include "misc.hh"
#include "midi-def.hh"
#include "translator.hh"
#include "performance.hh"
#include "assoc-iter.hh"
#include "score-performer.hh"
#include "debug.hh"

// classes, alphasorted
//     statics
//     constructors
//     destructor
//     routines, alphasorted

Midi_def::Midi_def()
{
  outfile_str_ = ""; 
  // ugh
  set_tempo (Moment (1, 4), 60);
}

Midi_def::Midi_def (Midi_def const& s)
  : Music_output_def (s)
{
  whole_seconds_f_ = s.whole_seconds_f_;
  outfile_str_ = s.outfile_str_;
}

Midi_def::~Midi_def()
{
}

Real
Midi_def::duration_to_seconds_f (Moment mom)
{
  if (!mom)
	return 0;
  
  return Moment (whole_seconds_f_) * mom;
}



int
Midi_def::get_tempo_i (Moment moment)
{
  return Moment (whole_seconds_f_) * Moment (60) * moment;
}

void
Midi_def::print() const
{
#ifndef NPRINT
  DOUT << "Midi {";
  DOUT << "4/min: " << Real (60) / (whole_seconds_f_ * 4);
  DOUT << "out: " << outfile_str_;
  DOUT << "}\n";
#endif
}


void
Midi_def::set_tempo (Moment moment, int count_per_minute_i)
{
  whole_seconds_f_ = Moment (count_per_minute_i) / Moment (60) / moment;
}

IMPLEMENT_IS_TYPE_B1( Midi_def, Music_output_def);
