#include "pitch.hh" 
#include "request-chord-iterator.hh"
#include "music-list.hh"
#include "musical-request.hh"

Request_chord::Request_chord ()
{
}

Pitch
Request_chord::to_relative_octave (Pitch last)
{
  for (SCM s = music_list (); gh_pair_p (s);  s = ly_cdr (s))
    {
      Music * mus = unsmob_music (ly_car (s));

      if (mus->is_mus_type ("melodic-event")
	  || mus->is_mus_type ("rest-event"))
	{
	  Pitch *old_pit = unsmob_pitch (mus->get_mus_property ("pitch"));
	  if (!old_pit)
	    continue;
	  
	  Pitch new_pit = *old_pit;
	  new_pit.to_relative_octave (last);
	  mus->set_mus_property ("pitch", new_pit.smobbed_copy ());

	  return new_pit;
	}

    }
  return last;
}

Moment
Request_chord::start_mom () const
{
  return Music::start_mom ();
}

ADD_MUSIC (Request_chord);
