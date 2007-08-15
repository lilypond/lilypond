/*
  grob-pitch-tuple.cc -- implement Grob_pitch_tuple

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grob-pitch-tuple.hh"

#include "music.hh"

int compare (Grob_pitch_tuple const &a, Grob_pitch_tuple const &b)
{
  return Grob_pitch_tuple::time_compare (a, b);
}

Grob_pitch_tuple::Grob_pitch_tuple ()
{
  head_ = 0;
  end_ = 0;
}

Grob_pitch_tuple::Grob_pitch_tuple (Grob *h, Music *m, Moment mom)
{
  head_ = h;
  pitch_ = *unsmob_pitch (m->get_property ("pitch"));
  end_ = mom;
}

/*
  signed compare, should use pitch<?
*/
int
Grob_pitch_tuple::pitch_compare (Grob_pitch_tuple const &h1,
				 Grob_pitch_tuple const &h2)
{
  return Pitch::compare (h1.pitch_, h2.pitch_);
}

int
Grob_pitch_tuple::time_compare (Grob_pitch_tuple const &h1,
				Grob_pitch_tuple const &h2)
{
  return Moment::compare (h1.end_, h2.end_);
}
