/*
  midi-def.cc -- implement Midi_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>

*/
#include <math.h>
#include "misc.hh"
#include "midi-def.hh"
#include "performance.hh"
#include "debug.hh"
#include "scope.hh"

Midi_def::Midi_def()
{
  // ugh
  set_tempo (Moment (1, 4), 60);
}

Midi_def::~Midi_def()
{
}



int
Midi_def::get_tempo_i (Moment one_beat_mom)
{
  Moment w = *unsmob_moment (scope_p_->scm_elem ("whole-in-seconds"));
  Moment wholes_per_min = Moment(60) /w;
  int beats_per_min = wholes_per_min / one_beat_mom;
  return int (beats_per_min);
}

void
Midi_def::set_tempo (Moment one_beat_mom, int beats_per_minute_i)
{
  Moment beats_per_second = Moment (beats_per_minute_i) / Moment (60);

  Moment *m = new Moment (Moment(1)/Moment(beats_per_second * one_beat_mom));
  scope_p_->set ("whole-in-seconds", smobify (m));
}


int Midi_def::default_count_i_=0;

int
Midi_def::get_next_default_count () const
{
  return default_count_i_++;
}

void
Midi_def::reset_default_count ()
{
  default_count_i_ = 0;
}
