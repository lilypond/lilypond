#include "pitch.hh" 
#include "request-chord-iterator.hh"
#include "music-list.hh"
#include "musical-request.hh"

Request_chord::Request_chord (SCM s)
  : Simultaneous_music (s)
{
  set_mus_property ("iterator-ctor",
		    Request_chord_iterator::constructor_cxx_function);
}

Request_chord::Request_chord ()
{
  set_mus_property ("iterator-ctor",
		    Request_chord_iterator::constructor_cxx_function);
}



Pitch
Request_chord::to_relative_octave (Pitch last)
{
  for (SCM s = music_list (); gh_pair_p (s);  s = gh_cdr (s))
    {
      Music * mus = unsmob_music (gh_car (s));
      if (Melodic_req *m= dynamic_cast <Melodic_req *> (mus))
	{
	  Pitch pit = *unsmob_pitch (m->get_mus_property ("pitch"));
	  
	  pit.to_relative_octave (last);
	  m->set_mus_property ("pitch", pit.smobbed_copy ());
	  	  
	  return pit;
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
