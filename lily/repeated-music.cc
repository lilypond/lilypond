/*   
  repeated-music.cc --  implement Repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "repeated-music.hh"
#include "music-list.hh"
#include "pitch.hh"
#include "warn.hh"
#include "music-sequence.hh"

Music *
Repeated_music::body ()const
{
  return unsmob_music (get_mus_property ("element"));
}

SCM
Repeated_music::alternatives ()const
{
  return get_mus_property ("elements");
}




Pitch
Repeated_music::to_relative_octave (Pitch p)
{
  if (body ())
    p = body ()->to_relative_octave (p);

  Pitch last = p ; 
  if (alternatives ())
    for (SCM s = alternatives (); gh_pair_p (s);  s = ly_cdr (s))
      unsmob_music (ly_car (s))->to_relative_octave (p);
     

  return last;
}

void
Repeated_music::transpose (Pitch p)
{
  if (body ())
    body ()->transpose (p);

  Music_sequence::transpose_list (get_mus_property ("elements"), p);
}

void
Repeated_music::compress (Moment p)
{
  if (body ())
    body ()->compress (p);

  Music_sequence::compress_list (alternatives (), p);
}

Moment
Repeated_music::alternatives_get_length (bool fold) const
{
  if (!alternatives ())
    return 0;
  
  if (fold)
    return Music_sequence::maximum_length (alternatives ());

  Moment m =0;
  int done =0;

  SCM p = alternatives ();
  while (gh_pair_p (p) && done < repeat_count ())
    {
      m = m + unsmob_music (ly_car (p))->get_length ();
      done ++;
      if (repeat_count () - done < scm_ilength (alternatives ()))
	p = ly_cdr (p);
    }
  return m;
}

/*
  Sum all duration of all available alternatives. This is for the case
  of volta repeats, where the alternatives are iterated just as they
  were entered.  */
Moment
Repeated_music::alternatives_volta_get_length () const
{
  if (!alternatives ())
    return 0;

  Moment m;
  SCM p = alternatives ();
  while (gh_pair_p (p))
    {
      m = m + unsmob_music (ly_car (p))->get_length ();
      p = ly_cdr (p);
    }
  return m;
}


/*
  Length of the body in THIS. Disregards REPEAT-COUNT. 
 */
Moment
Repeated_music::body_get_length () const
{
  Moment m = 0;
  if (body ())
    {
      m = body ()->get_length ();
    }
  return m;
}

int
Repeated_music::repeat_count () const
{
  return gh_scm2int (get_mus_property ("repeat-count"));
}


MAKE_SCHEME_CALLBACK (Repeated_music,unfolded_music_length, 1);
MAKE_SCHEME_CALLBACK (Repeated_music,folded_music_length, 1);
MAKE_SCHEME_CALLBACK (Repeated_music,volta_music_length, 1);

SCM
Repeated_music::unfolded_music_length (SCM m)
{
  Repeated_music* r = dynamic_cast<Repeated_music*> (unsmob_music (m));
  
  Moment l = Moment (r->repeat_count ()) * r->body_get_length () + r->alternatives_get_length (false);
  return l.smobbed_copy ();
}

SCM
Repeated_music::folded_music_length (SCM m)
{
  Repeated_music* r = dynamic_cast<Repeated_music*> (unsmob_music (m));
 
  Moment l =  r->body_get_length () + r->alternatives_get_length (true);
  return l.smobbed_copy ();
}

SCM
Repeated_music::volta_music_length (SCM m)
{
  Repeated_music* r = dynamic_cast<Repeated_music*> (unsmob_music (m));
  Moment l =  r->body_get_length () + r->alternatives_volta_get_length ();
  return l.smobbed_copy ();
}

ADD_MUSIC (Repeated_music);

Repeated_music::Repeated_music ()
  : Music ()
{
}


MAKE_SCHEME_CALLBACK (Repeated_music,minimum_start, 1);
MAKE_SCHEME_CALLBACK (Repeated_music,first_start, 1);

SCM
Repeated_music::minimum_start (SCM m)
{
  Music * me = unsmob_music (m);
  Music * body = unsmob_music (me->get_mus_property ("element"));

  if (body)
    return body->start_mom ().smobbed_copy();
  else
    {
      return Music_sequence::minimum_start (me->get_mus_property ("elements")).smobbed_copy();
    }
}

SCM
Repeated_music::first_start (SCM m)
{
  Music * me = unsmob_music (m);
  Music * body = unsmob_music (me->get_mus_property ("element"));

  Moment rv =  (body) ? body->start_mom () :
    Music_sequence::first_start (me->get_mus_property ("elements"));

  return rv.smobbed_copy ();
}
