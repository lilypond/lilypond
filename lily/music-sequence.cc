/*   
  music-sequence.cc --  implement Music_sequence
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "music-list.hh"
#include "warn.hh"
#include "pitch.hh"


SCM
Music_sequence::music_list ()const
{
  return get_mus_property ("elements");
}

/*
  Ugh this sucks. Linear. do not use.
 */
void
Music_sequence::append_music (Music *m)
{
  set_mus_property ("elements",
		    gh_append2 (music_list (), gh_cons (m->self_scm (), SCM_EOL)));
  scm_gc_unprotect_object (m->self_scm ());
}

Music_sequence::Music_sequence ( )
  : Music ()
{
}

void
Music_sequence::transpose (Pitch rq)
{
  transpose_list (music_list (), rq);
}

void
Music_sequence::transpose_list (SCM l,  Pitch rq)
{
  for (SCM s = l; gh_pair_p (s);  s = ly_cdr (s))
    unsmob_music (ly_car (s))->transpose (rq);    
}

Moment
Music_sequence::cumulative_length (SCM l) 
{
  Moment cumulative;
  Moment last_len; 

  for (SCM s = l; gh_pair_p (s);  s = ly_cdr (s))
    {
      Moment l = unsmob_music (ly_car (s))->get_length ();
      if (last_len.grace_part_ && l.main_part_)
	{
	  last_len.grace_part_ = Rational (0);
	}
      cumulative += last_len;
      last_len = l;
    }

  last_len.grace_part_ = Rational (0);
  cumulative += last_len;

  return  cumulative;
}

Pitch
Music_sequence::to_relative_octave (Pitch p)
{
  return do_relative_octave (p, false);
}


Moment
Music_sequence::maximum_length (SCM l)
{
  Moment dur = 0;
  for (SCM s = l; gh_pair_p (s);  s = ly_cdr (s))
    {
      Music * m = unsmob_music (ly_car (s));
      Moment l = m->get_length ();
      dur = dur >? l;
    }

  return dur;
}


Pitch
Music_sequence::do_relative_octave (Pitch p, bool ret_first)
{
  Pitch retval;
  int count=0;

  Pitch last = p;
  for (SCM s = music_list (); gh_pair_p (s);  s = ly_cdr (s))
    {
      Music *m = unsmob_music (ly_car (s));
      if (!m)
	{
	  programming_error ("Music_sequence should only contain music!");
	}
      else
	{
	  last = m->to_relative_octave (last);
	  if (!count ++)
	    retval = last;
	}
    }

  if (!ret_first)
    retval = last;
  
  return retval;
}

void
Music_sequence::compress (Moment m)
{
  compress_list (music_list (), m);
}

void
Music_sequence::compress_list (SCM l, Moment m)
{
  for (SCM s = l; gh_pair_p (s);  s = ly_cdr (s))
    unsmob_music (ly_car (s))->compress (m);
}

ADD_MUSIC (Music_sequence);

Moment
Music_sequence::minimum_start (SCM l)
{
  Moment m;
  
  for (SCM s = l; gh_pair_p (s);  s = ly_cdr (s))
    {
      m = m <? unsmob_music (ly_car (s))->start_mom ();
    }
  return m;
}

Moment
Music_sequence::first_start (SCM l) 
{
  Moment m;
  
  for (SCM s = l; gh_pair_p (s);  s = ly_cdr (s))
    {
      Music * mus = unsmob_music (ly_car (s));
      Moment l = mus->get_length ();
      Moment s = mus->start_mom ();
      if (l.to_bool () || s.to_bool ())
	return s;
    }
  return m;
}

