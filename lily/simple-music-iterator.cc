/*   
  simple-music-iterator.cc --  implement Simple_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  if (music_l_)
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
#if 0
  /*
    try_music () causes trouble for base classes
   */
  if (music_l_)
    {
      bool b = try_music (music_l_);
      if (!b)
	music_l_->origin ()->warning (_f ("Junking music: `%s'",
					  classname (music_l_)));
    }
#endif
  skip (m);
}
