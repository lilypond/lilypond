/*   
  time-scaled-music-iterator.cc -- implement Time_scaled_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "time-scaled-music-iterator.hh"
#include "time-scaled-music.hh"
#include "musical-request.hh"
#include "translator-group.hh"
#include "command-request.hh"

void
Time_scaled_music_iterator::process (Moment m)
{
  if (!m)
    {
      Music_iterator *yeah = try_music (music_l_);
      if (yeah)
	set_translator (yeah->report_to_l ());
      else
	music_l_->origin ()->warning ( _("no one to print a tuplet start bracket"));
    }

  Music_wrapper_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK(Time_scaled_music_iterator);
