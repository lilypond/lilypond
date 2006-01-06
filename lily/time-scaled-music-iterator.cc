/*
  time-scaled-music-iterator.cc -- implement Time_scaled_music_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "time-scaled-music-iterator.hh"
#include "context.hh"
#include "input.hh"

void
Time_scaled_music_iterator::process (Moment m)
{
  if (!m.to_bool ())
    {
      Music_iterator *yeah = try_music (get_music ());
      if (yeah)
	set_context (yeah->get_outlet ());
      else
	get_music ()->origin ()->warning (_ ("no one to print a tuplet start bracket"));
    }

  Music_wrapper_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Time_scaled_music_iterator);
