/*   
  output-property-music-iterator.cc -- implement Output_property_music_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2004 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "input.hh"
#include "simple-music-iterator.hh"
#include "music.hh"

class Output_property_music_iterator : public Simple_music_iterator
{
public:  
  DECLARE_SCHEME_CALLBACK (constructor, ());
protected:
  virtual void process (Moment);
};


void
Output_property_music_iterator::process (Moment m)
{
  if (last_processed_mom_ < Moment (0))
    {
      bool accepted = try_music (get_music ());
      if (!accepted)
	get_music ()->origin ()->warning (_f ("Junking event: `%s'",
					  classname (get_music ())));
    }
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Output_property_music_iterator);
