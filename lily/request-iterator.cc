/*   
  request-iterator.cc --  implement Simple_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "request-iterator.hh"
#include "music.hh"


void
Simple_music_iterator::do_process_and_next (Moment m)
{
  if (first_b_)
    {
      bool g= try_music (music_l_);
      if (!g)
	music_l_->warning (_f ("Junking music: `%s'", classname(music_l_)));

      first_b_ = false;
    }
  Music_iterator::do_process_and_next (m);
}
