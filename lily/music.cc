/*
  music.cc -- implement Music

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music.hh"
#include "music-list.hh"
#include "debug.hh"
#include "musical-pitch.hh"

Music::Music()
{
}

void    
Music::compress (Moment)
{
}

void
Music::do_print() const
{
}

Moment
Music::length_mom () const
{
  return 0;
}

void
Music::print() const
{
#ifndef NPRINT
  if (! flower_dstream)
    return ;
  DEBUG_OUT << classname(this) << "{";
  
  do_print();
  DEBUG_OUT << "}\n";
#endif
}

Musical_pitch
Music::to_relative_octave (Musical_pitch m)
{
  return m;
}


void
Music::transpose (Musical_pitch )
{
}

