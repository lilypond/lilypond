/*   
  simple-music-iterator.cc --  implement Simple_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "simple-music-iterator.hh"
#include "music.hh"
#include "input.hh"


void
Simple_music_iterator::do_process_and_next (Moment m)
{
  if (ok ())
    {
      bool b = try_music (get_music ());
      if (!b)
	music_l_->origin ()->warning (_f ("Junking music: `%s'",
					  classname (music_l_)));

    }
  Music_iterator::do_process_and_next (m);
}
