/*   
  new-repeated-music.cc --  implement New_repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "new-repeated-music.hh"
#include "music-list.hh"
#include "musical-pitch.hh"

New_repeated_music::New_repeated_music()
{
  repeat_begin_p_ = 0;
  unfold_b_ = false;
  repeats_i_ =0;
  alternatives_p_ = 0;
}

New_repeated_music::New_repeated_music (New_repeated_music const &s)
  : Music (s)
{
  repeats_i_ = s.repeats_i_;
  unfold_b_ = s.unfold_b_;

  repeat_begin_p_ = s.repeat_begin_p_ ? s.repeat_begin_p_->clone () : 0;
  alternatives_p_ = s.alternatives_p_
    ? dynamic_cast<Music_sequence*> (s.alternatives_p_->clone ()):0;
}

New_repeated_music::~New_repeated_music ()
{
  delete repeat_begin_p_;
  delete alternatives_p_;
}

void
New_repeated_music::do_print () const
{
  if (repeat_begin_p_)
    repeat_begin_p_->print();
  
  if (alternatives_p_)
    alternatives_p_->print();
}

Musical_pitch
New_repeated_music::to_relative_octave (Musical_pitch p)
{
  if (repeat_begin_p_)
    p = repeat_begin_p_->to_relative_octave (p);

  if (alternatives_p_)
    p = alternatives_p_->do_relative_octave (p, true);
  return p;
}


void
New_repeated_music::transpose (Musical_pitch p)
{
  if (repeat_begin_p_)
    repeat_begin_p_->transpose (p);

  if (alternatives_p_)
    alternatives_p_->transpose (p);  
}

void
New_repeated_music::compress (Moment p)
{
  if (repeat_begin_p_)
    repeat_begin_p_->compress (p);

  if (alternatives_p_)
    alternatives_p_->compress (p);  
}


Moment
New_repeated_music::length_mom () const
{
  Moment m =0;
  if (unfold_b_)
    {
      if (repeat_begin_p_)
	m +=  Rational (repeats_i_) * repeat_begin_p_->length_mom ();

      if (alternatives_p_)
	m += alternatives_p_->cumulative_length ();
    }
  else
    {
      if (repeat_begin_p_)
	m +=  repeat_begin_p_->length_mom ();

      if (alternatives_p_)
	m += alternatives_p_->maximum_length ();
    }
  return m;
}

