/*   
  music-sequence.cc --  implement Music_sequence
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "music-list.hh"
#include "debug.hh"
#include "musical-pitch.hh"


void
Music_sequence::truncate (int k)
{
  SCM l = get_mus_property ("list");
  if (k == 0)
    {
      l = SCM_EOL;
    }
  else
    {
      SCM s = l;
      k--;
      for (; gh_pair_p (s) && k--; s = gh_cdr (s))
	;

      if (gh_pair_p (s))
	{
	  gh_set_cdr_x (s, SCM_EOL);
	}
    }
  set_mus_property ("list", l);
}

SCM
Music_sequence::music_list ()const
{
  return get_mus_property ("list");
}

/*
  Ugh this sucks. Linear. do not use.
 */
void
Music_sequence::append_music (Music *m)
{
  set_mus_property ("list",
		    gh_append2( music_list(), gh_cons (m->self_scm (), SCM_EOL)));
  scm_unprotect_object (m->self_scm ());
}

Music_sequence::Music_sequence(SCM h)
{
  set_mus_property ("list", h);
}

void
Music_sequence::transpose (Musical_pitch rq)
{
  for (SCM s = music_list (); gh_pair_p (s);  s = gh_cdr (s))
    unsmob_music (gh_car (s))->transpose (rq);    
}




Moment
Music_sequence::cumulative_length () const
{
  Moment last=0;
  for (SCM s = music_list (); gh_pair_p (s);  s = gh_cdr (s))
    last += unsmob_music (gh_car (s))->length_mom ();
  return  last;
}

Musical_pitch
Music_sequence::to_relative_octave (Musical_pitch p)
{
  return do_relative_octave (p, false);
}


Moment
Music_sequence::maximum_length () const
{
  Moment dur = 0;
  for (SCM s = music_list (); gh_pair_p (s);  s = gh_cdr (s))
    dur = dur >? unsmob_music (gh_car (s))->length_mom ();

  return dur;
}
int
Music_sequence::length_i () const
{
  return scm_ilength (music_list ());
}

Musical_pitch
Music_sequence::do_relative_octave (Musical_pitch p, bool ret_first)
{
  Musical_pitch retval;
  int count=0;

  Musical_pitch last = p;
  for (SCM s = music_list (); gh_pair_p (s);  s = gh_cdr (s))
    {
      last = unsmob_music (gh_car (s))->to_relative_octave (last);
      if (!count ++ )
	retval = last;
    }

  if (!ret_first)
    retval = last;
  
  return retval;
}

void
Music_sequence::compress (Moment m)
{
  for (SCM s = music_list (); gh_pair_p (s);  s = gh_cdr (s))
    unsmob_music (gh_car (s))->compress (m);
}
