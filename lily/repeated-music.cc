/*   
  repeated-music.cc --  implement Repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "repeated-music.hh"
#include "music-list.hh"
#include "musical-pitch.hh"
#include "debug.hh"

Repeated_music::Repeated_music(Music *beg, int times, Music_sequence * alts)
{
  repeat_body_p_ = beg;
  fold_b_ = false;
  repeats_i_ = times;
  alternatives_p_ = alts;
  volta_fold_b_ = true;
  if (alts)
    alts->music_p_list_p_->truncate (times);
}

Repeated_music::Repeated_music (Repeated_music const &s)
  : Music (s)
{
  repeats_i_ = s.repeats_i_;
  fold_b_ = s.fold_b_;
  volta_fold_b_ = s.volta_fold_b_;
  type_ = s.type_;
  repeat_body_p_ = s.repeat_body_p_ ? s.repeat_body_p_->clone () : 0;
  alternatives_p_ = s.alternatives_p_
    ? dynamic_cast<Music_sequence*> (s.alternatives_p_->clone ()):0;
}

Repeated_music::~Repeated_music ()
{
  delete repeat_body_p_;
  delete alternatives_p_;
}

void
Repeated_music::do_print () const
{
#ifndef NPRINT
  DEBUG_OUT << "Fold = " << fold_b_ << " reps: " << repeats_i_;

  if (repeat_body_p_)
    repeat_body_p_->print();
  
  if (alternatives_p_)
    alternatives_p_->print();
#endif
}

Musical_pitch
Repeated_music::to_relative_octave (Musical_pitch p)
{
  if (repeat_body_p_)
    p = repeat_body_p_->to_relative_octave (p);

  Musical_pitch last = p ; 
  if (alternatives_p_)
    for (Cons<Music> *i = alternatives_p_->music_p_list_p_->head_; i ; i = i->next_)
      {
	last = i->car_->to_relative_octave (p);
      }

  return last;
}



void
Repeated_music::transpose (Musical_pitch p)
{
  if (repeat_body_p_)
    repeat_body_p_->transpose (p);

  if (alternatives_p_)
    alternatives_p_->transpose (p);  
}

void
Repeated_music::compress (Moment p)
{
  if (repeat_body_p_)
    repeat_body_p_->compress (p);

  if (alternatives_p_)
    alternatives_p_->compress (p);  
}

Moment
Repeated_music::alternatives_length_mom () const
{
  if (!alternatives_p_ )
    return 0;
  
  if  (fold_b_)
    return alternatives_p_->maximum_length ();

  Moment m =0;
  int done =0;
  Cons<Music> *p = alternatives_p_->music_p_list_p_->head_;
  while (p && done < repeats_i_)
    {
      m = m + p->car_->length_mom ();
      done ++;
      if (volta_fold_b_
	  || repeats_i_ - done < alternatives_p_->length_i ())
	p = p->next_;
    }
  return m;
}

Moment
Repeated_music::body_length_mom () const
{
  Moment m = 0;
  if (repeat_body_p_)
    {
      m = repeat_body_p_->length_mom ();
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

