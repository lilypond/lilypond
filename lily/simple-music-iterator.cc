/*   
  simple-music-iterator.cc --  implement Simple_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "simple-music-iterator.hh"
#include "music.hh"
#include "input.hh"

Simple_music_iterator::Simple_music_iterator ()
  : Music_iterator ()
{
  last_processed_mom_ = -1;
}

Simple_music_iterator::Simple_music_iterator (Simple_music_iterator const &src)
  : Music_iterator (src)
{
  last_processed_mom_ = src.last_processed_mom_;
}

bool
Simple_music_iterator::ok ()const
{
  return last_processed_mom_ < music_length_mom ();
}

Moment
Simple_music_iterator::pending_moment ()const
{
  if (last_processed_mom_ < Moment (0))
    return Moment (0);
  else
    return music_length_mom ();
}

void
Simple_music_iterator::skip (Moment m)
{
  music_l_ = 0;
  last_processed_mom_ = m;
}

void
Simple_music_iterator::process (Moment m)
{
  /*
  don't do try_music (), since it would make the function useless for
  base classes */

  skip (m);
}

IMPLEMENT_CTOR_CALLBACK (Simple_music_iterator);
