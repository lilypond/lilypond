/*
  music.cc -- implement Music

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "music.hh"
#include "music-list.hh"
#include "debug.hh"
#include "musical-pitch.hh"

Moment
Music::duration () const
{
  return 0;
}

void
Music::print() const
{
#ifndef NPRINT
  if (! check_debug)
    return ;
  DOUT << name() << "{";
  if  (translator_type_str_.length_i ())
    DOUT << translator_type_str_ << " = " << translator_id_str_;
  do_print();
  DOUT << "}\n";
#endif
}

void
Music::transpose (Musical_pitch )
{
  
}

void
Music::do_print() const
{
}


IMPLEMENT_IS_TYPE_B(Music);



  
Music::Music()
{
}

Musical_pitch
Music::to_relative_octave (Musical_pitch m)
{
  return m;
}

void    
Music::compress (Moment)
{
}
