/*   
  request-iterator.cc --  implement 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "request-iterator.hh"
#include "music.hh"

Request_iterator::Request_iterator()
{
}


void
Request_iterator::do_process_and_next (Moment m)
{
  if (first_b_)
    {
      bool g= try_music (music_l_);
      if (!g)
	music_l_->warning (_f ("Junking request: `%s'", classname(music_l_)));

      first_b_ = false;
    }
  Music_iterator::do_process_and_next (m);
}
