/*   
  output-property-music-iterator.cc -- implement Output_property_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "input.hh"
#include "music-list.hh"
#include "output-property-music-iterator.hh"

void
Output_property_music_iterator::process (Moment m)
{
  if (last_processed_mom_ < Moment (0))
    {
      bool accepted = try_music (music_l ());
      if (!accepted)
	music_l ()->origin ()->warning (_f ("Junking request: `%s'",
					  classname (music_l ())));
    }

  skip (m);
}

IMPLEMENT_CTOR_CALLBACK (Output_property_music_iterator);
