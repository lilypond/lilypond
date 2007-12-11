/*
  pitch-interval.cc -- implement Pitch_interval

  source file of the GNU LilyPond music typesetter

  (c) 2004--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "pitch-interval.hh"

#include "interval.tcc"

Pitch_interval::Pitch_interval (Pitch p1, Pitch p2)
{
  at (LEFT) = p1;
  at (RIGHT) = p2;
}

Pitch_interval::Pitch_interval ()
{
  at (LEFT) = Pitch (100, 0, 0);
  at (RIGHT) = Pitch (-100, 0, 0);
}

bool
Pitch_interval::is_empty () const
{
  return at (LEFT) > at (RIGHT);
}

Direction
Pitch_interval::add_point (Pitch p)
{
  if (at (LEFT).tone_pitch () > p.tone_pitch ())
    {
      at (LEFT) = p;
      return LEFT;
    }
  else if (at (RIGHT).tone_pitch () < p.tone_pitch ())
    {
      at (RIGHT) = p;
      return RIGHT;
    }
  else
    return CENTER;
}


Pitch_lexicographic_interval::Pitch_lexicographic_interval (Pitch p1, Pitch p2)
{
  at (LEFT) = p1;
  at (RIGHT) = p2;
}

Pitch_lexicographic_interval::Pitch_lexicographic_interval ()
{
  at (LEFT) = Pitch (100, 0, 0);
  at (RIGHT) = Pitch (-100, 0, 0);
}

bool
Pitch_lexicographic_interval::is_empty () const
{
  return at (LEFT) > at (RIGHT);
}

Direction
Pitch_lexicographic_interval::add_point (Pitch p)
{
  if (at (LEFT) > p)
    {
      at (LEFT) = p;
      return LEFT;
    }
  else if (at (RIGHT) < p)
    {
      at (RIGHT) = p;
      return RIGHT;
    }
  else
    return CENTER;
}
