#include "input.hh"
#include "moment.hh"
#include "pitch.hh"
#include "music-list.hh"

Moment
Simultaneous_music::get_length () const
{
  return Music_sequence::maximum_length (get_mus_property ("elements"));
}

Moment
Simultaneous_music::start_mom () const
{
  return Music_sequence::minimum_start (get_mus_property ("elements"));
}

Simultaneous_music::Simultaneous_music()
{

}

/*
  Cut & paste from Music_sequence, (ugh) , but we must add an error
  message.
 */
Pitch
Simultaneous_music::to_relative_octave (Pitch p)
{
  Pitch first;
  int count=0;

  Pitch last = p;
  for (SCM s = music_list (); gh_pair_p (s);  s = ly_cdr (s))
    {
      if (Music *m = unsmob_music (ly_car (s)))
	{
	  last = m->to_relative_octave (last);
	  if (!count ++)
	    first = last;
	}
    }

  if (count && first != last)
    {
      String str = _("Changing relative definition may cause octave change.");
      str += "\nWas: " +  first.to_string ()
	+ " -- now returning: " + last.to_string () + "\n";
      
      origin()->warning (str);
    }

  return last;
}

ADD_MUSIC (Simultaneous_music);

Pitch
Event_chord::to_relative_octave (Pitch p)
{
  return do_relative_octave (p, true);
}
ADD_MUSIC(Event_chord);
