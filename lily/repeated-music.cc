/*   
  repeated-music.cc --  implement Repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "repeated-music.hh"
#include "music-list.hh"
#include "musical-pitch.hh"
#include "debug.hh"

Music *
Repeated_music::body ()const
{
  return unsmob_music (get_mus_property ("body"));
}

Music_sequence*
Repeated_music::alternatives ()const
{
  return dynamic_cast<Music_sequence*>  (unsmob_music (get_mus_property ("alternatives")));
}

Repeated_music::Repeated_music(Music *beg, int times, Music_sequence * alts)
{
  set_mus_property ("body", beg->self_scm_);
  fold_b_ = false;
  repeats_i_ = times;
  volta_fold_b_ = true;
  if (alts)
    {
      alts->truncate (times);
      set_mus_property ("alternatives", alts->self_scm_);
    }
}

Repeated_music::Repeated_music (Repeated_music const &s)
  : Music (s)
{
  repeats_i_ = s.repeats_i_;
  fold_b_ = s.fold_b_;
  volta_fold_b_ = s.volta_fold_b_;
  type_ = s.type_;
}


void
Repeated_music::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << "Fold = " << fold_b_ << " reps: " << repeats_i_;

  if (body ())
    body ()->print();
  
  if (alternatives ())
    alternatives ()->print();
#endif
}

Musical_pitch
Repeated_music::to_relative_octave (Musical_pitch p)
{
  if (body ())
    p = body ()->to_relative_octave (p);

  Musical_pitch last = p ; 
  if (alternatives ())
    for (SCM s = alternatives ()->music_list (); gh_pair_p (s);  s = gh_cdr (s))
      unsmob_music (gh_car (s))->to_relative_octave (p);
     

  return last;
}

void
Repeated_music::transpose (Musical_pitch p)
{
  if (body ())
    body ()->transpose (p);

  if (alternatives ())
    alternatives ()->transpose (p);  
}

void
Repeated_music::compress (Moment p)
{
  if (body ())
    body ()->compress (p);

  if (alternatives ())
    alternatives ()->compress (p);  
}

Moment
Repeated_music::alternatives_length_mom () const
{
  if (!alternatives () )
    return 0;
  
  if  (fold_b_)
    return alternatives ()->maximum_length ();

  Moment m =0;
  int done =0;

  SCM p = alternatives ()->music_list ();
   while (gh_pair_p (p) && done < repeats_i_)
    {
      m = m + unsmob_music (gh_car (p))->length_mom ();
      done ++;
      if (volta_fold_b_
	  || repeats_i_ - done < alternatives ()->length_i ())
      p = gh_cdr (p);
    }
  return m;
}

Moment
Repeated_music::body_length_mom () const
{
  Moment m = 0;
  if (body ())
    {
      m = body ()->length_mom ();
      if (!fold_b_ && !volta_fold_b_)
	m *= Rational (repeats_i_);
    }
  return m;
}

Moment
Repeated_music::length_mom () const
{
  return body_length_mom () + alternatives_length_mom ();
}

