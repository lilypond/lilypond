/*
  pitch-interval.cc -- implement Pitch_interval

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "pitch-interval.hh"

#include "interval.tcc"

Pitch_interval::Pitch_interval (Pitch p1, Pitch p2)
{
  elem_ref (LEFT) = p1;
  elem_ref (RIGHT) = p2;
}

Pitch_interval::Pitch_interval ()
{
  elem_ref (LEFT) = Pitch (100, 0, 0);
  elem_ref (RIGHT) = Pitch (-100, 0, 0);
}

bool
Pitch_interval::is_empty () const
{
  return elem (LEFT) > elem (RIGHT);
}

void
Pitch_interval::add_point (Pitch p)
{
  if (elem_ref (LEFT) > p)
    elem_ref (LEFT) = p;
  if (elem_ref (RIGHT) < p)
    elem_ref (RIGHT) = p;
}
