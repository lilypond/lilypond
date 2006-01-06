/*
  midi-def.cc -- implement midi output def functions

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/


#include "misc.hh"
#include "output-def.hh"
#include "moment.hh"
#include "warn.hh"
#include "scm-hash.hh"

int
get_tempo (Output_def *def,
	   Moment one_beat_mom)
{
  SCM wis = ly_symbol2scm ("whole-in-seconds");
  Moment *w = unsmob_moment (def->lookup_variable (wis));

  Moment wholes_per_min = Moment (60);
  if (!w)
    {
      programming_error ("wholes-in-seconds not set.");
      wholes_per_min /= 4;
    }
  else
    wholes_per_min /= *w;

  Rational beats_per_min =  (wholes_per_min / one_beat_mom).main_part_;
  return beats_per_min.to_int ();
}

void
set_tempo (Output_def *def,
	   Moment one_beat_mom,
	   int beats_per_minute_i)
{
  Moment beats_per_second = Moment (beats_per_minute_i) / Moment (60);

  Moment m = Moment (1) / Moment (beats_per_second * one_beat_mom);
  def->set_variable (ly_symbol2scm ("whole-in-seconds"), m.smobbed_copy ());
}

