/*
  midi-def.cc -- implement Midi_def

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>

*/
#include <math.h>
#include "misc.hh"
#include "midi-def.hh"
#include "performance.hh"
#include "debug.hh"
#include "scope.hh"

Midi_def::Midi_def ()
{
  // ugh
  set_tempo (Moment (Rational (1, 4)), 60);
}

int
Midi_def::get_tempo_i (Moment one_beat_mom)
{
  SCM wis  = ly_symbol2scm ("whole-in-seconds");
  Moment *w = unsmob_moment (scope_p_->scm_elem (wis));

  Moment wholes_per_min = Moment (60);
  if (!w)
    {
      programming_error  ("wholes-in-seconds not set.");
      wholes_per_min /= 4;
    }
  else
    {
      wholes_per_min /= *w; 
    }
  
  int beats_per_min =  int ((wholes_per_min / one_beat_mom).main_part_);
  return int (beats_per_min);
}

void
Midi_def::set_tempo (Moment one_beat_mom, int beats_per_minute_i)
{
  Moment beats_per_second = Moment (beats_per_minute_i) / Moment (60);

  Moment m = Moment (1)/Moment (beats_per_second * one_beat_mom);
  scope_p_->set ("whole-in-seconds", m.smobbed_copy ());
}


int Midi_def::score_count_i_=0;

int
Midi_def::get_next_score_count () const
{
  return score_count_i_++;
}

void
Midi_def::reset_score_count ()
{
  score_count_i_ = 0;
}
