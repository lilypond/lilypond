/*   
  new-repeated-music.cc --  implement New_repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "new-repeated-music.hh"
#include "music-list.hh"
#include "musical-pitch.hh"
#include "debug.hh"

New_repeated_music::New_repeated_music(Music *beg, int times, Music_sequence * alts)
{
  repeat_body_p_ = beg;
  fold_b_ = false;
  repeats_i_ = times;
  alternatives_p_ = alts;
  semi_fold_b_ = true;
}

New_repeated_music::New_repeated_music (New_repeated_music const &s)
  : Music (s)
{
  repeats_i_ = s.repeats_i_;
  fold_b_ = s.fold_b_;
  semi_fold_b_ = s.semi_fold_b_;
  
  repeat_body_p_ = s.repeat_body_p_ ? s.repeat_body_p_->clone () : 0;
  alternatives_p_ = s.alternatives_p_
    ? dynamic_cast<Music_sequence*> (s.alternatives_p_->clone ()):0;
}

New_repeated_music::~New_repeated_music ()
{
  delete repeat_body_p_;
  delete alternatives_p_;
}

void
New_repeated_music::do_print () const
{
#ifndef NPRINT
  DOUT << "Fold = " << fold_b_ << " reps: " << repeats_i_;

  if (repeat_body_p_)
    repeat_body_p_->print();
  
  if (alternatives_p_)
    alternatives_p_->print();
#endif
}

Musical_pitch
New_repeated_music::to_relative_octave (Musical_pitch p)
{
  if (repeat_body_p_)
    p = repeat_body_p_->to_relative_octave (p);

  if (alternatives_p_)
    p = alternatives_p_->do_relative_octave (p, true);
  return p;
}


void
New_repeated_music::transpose (Musical_pitch p)
{
  if (repeat_body_p_)
    repeat_body_p_->transpose (p);

  if (alternatives_p_)
    alternatives_p_->transpose (p);  
}

void
New_repeated_music::compress (Moment p)
{
  if (repeat_body_p_)
    repeat_body_p_->compress (p);

  if (alternatives_p_)
    alternatives_p_->compress (p);  
}

Moment
New_repeated_music::alternatives_length_mom () const
{
  if (alternatives_p_)
    {
      return  (fold_b_)
	? alternatives_p_->maximum_length ()
	: alternatives_p_->cumulative_length ();
    }
  return 0; 
}

Moment
New_repeated_music::length_mom () const
{
  Moment m =0;
  if (fold_b_)
    {
      if (repeat_body_p_)
	m += repeat_body_p_->length_mom ();
    }
  else
    {
      Moment beg = (repeat_body_p_) ? repeat_body_p_->length_mom () : Rational(0);
      if (!semi_fold_b_)
	beg *=  Rational (repeats_i_);
      m += beg;
    }

  m += alternatives_length_mom ();
  return m;
}

