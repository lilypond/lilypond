/*   
  repeated-music.cc --  implement Repeated_music
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "repeated-music.hh"
#include "musical-pitch.hh"

//Repeated_music::Repeated_music (Music* r, int n, Music_list* a)
Repeated_music::Repeated_music (Music* r, int n, Sequential_music* a)
{
  repeats_i_ = n;
  repeat_p_ = r;
  alternative_p_ = a;
}

Repeated_music::~Repeated_music ()
{
  delete repeat_p_;
  delete alternative_p_;
}

Repeated_music::Repeated_music (Repeated_music const& s)
  : Music (s)
{
  repeat_p_ = (s.repeat_p_) ? s.repeat_p_->clone () : 0;
  // urg?
  alternative_p_ = (s.alternative_p_) ? dynamic_cast <Sequential_music*> (s.alternative_p_->clone ()) : 0;
}

void
Repeated_music::do_print () const
{
  if (repeat_p_)
    repeat_p_->print ();
  if (alternative_p_)
    alternative_p_->print ();
}

void
Repeated_music::transpose (Musical_pitch p)
{
  if (repeat_p_)
    repeat_p_->transpose (p);
  if (alternative_p_)
    alternative_p_->transpose (p);
}

Moment
Repeated_music::duration () const
{
  Moment m;
  if (repeat_p_)
    m += repeat_p_->duration ();
  if (alternative_p_)
    m += alternative_p_->duration ();
  return m;
}

