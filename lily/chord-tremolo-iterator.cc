/*   
  chord-tremolo-iterator.cc --  implement Chord_tremolo_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


/*
  this is culled from various other iterators, but sharing code by subclassing proved to be too difficult.
 */

#include "input.hh"
#include "chord-tremolo-iterator.hh"
#include "repeated-music.hh"

void
Chord_tremolo_iterator::construct_children ()
{
  Repeated_music * rep = dynamic_cast<Repeated_music*> (music_l_);
  factor_  = Moment (1, rep->repeat_count ());
  child_iter_p_ = get_iterator_p (rep->body ());
}

Chord_tremolo_iterator::Chord_tremolo_iterator ()
{
  factor_ = 1;
  child_iter_p_ = 0;
}

Chord_tremolo_iterator::Chord_tremolo_iterator (Chord_tremolo_iterator const &src)
  : Music_iterator (src)
{
  child_iter_p_ = src.child_iter_p_? src.child_iter_p_->clone () : 0; 
}

void
Chord_tremolo_iterator::process (Moment m)
{
  if (!m)
    {
      Music_iterator *yeah = try_music (music_l_);
      if (yeah)
	set_translator (yeah->report_to_l ());
      else
	music_l_->origin ()->warning (_ ("no one to print a tremolos"));
    }

  child_iter_p_->process (factor_ * m);
}


Moment
Chord_tremolo_iterator::pending_moment () const
{
  return child_iter_p_->pending_moment () / factor_;
}

bool
Chord_tremolo_iterator::ok () const
{
  return child_iter_p_ && child_iter_p_->ok ();
}

Chord_tremolo_iterator::~Chord_tremolo_iterator ()
{
  delete child_iter_p_;
}

Music_iterator*
Chord_tremolo_iterator::try_music_in_children (Music  *m) const
{
  return child_iter_p_->try_music (m);
}



IMPLEMENT_CTOR_CALLBACK (Chord_tremolo_iterator);

