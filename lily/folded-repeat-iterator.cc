/*   
  repeated-music-iterator.cc --  implement Folded_repeat_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


/*
   Folded repeats are a stupid idea at this point, so we refrain from
   implementing get_pending_events () and skip ().
*/

#include "folded-repeat-iterator.hh"
#include "repeated-music.hh"
#include "music-list.hh"
#include "simultaneous-music-iterator.hh"
#include "translator-group.hh"

Folded_repeat_iterator::Folded_repeat_iterator ()
{
  main_iter_p_ = 0;
  alternative_iter_p_ = 0;
}

bool
Folded_repeat_iterator::ok () const
{
  return main_iter_p_ || alternative_iter_p_;
}

Folded_repeat_iterator::~Folded_repeat_iterator ()
{
  delete main_iter_p_;
  delete alternative_iter_p_;
}

Folded_repeat_iterator::Folded_repeat_iterator (Folded_repeat_iterator const &src)
  : Music_iterator (src)
{
  main_iter_p_ = src.main_iter_p_ ? src.main_iter_p_->clone () : 0;
  alternative_iter_p_ = src.alternative_iter_p_ ? src.alternative_iter_p_->clone () : 0;
  main_length_mom_ = src.main_length_mom_;
}

Moment
Folded_repeat_iterator::pending_moment () const
{
  if (main_iter_p_)
    {
      return main_iter_p_->pending_moment ();
    }
  else
    return main_length_mom_ + alternative_iter_p_->pending_moment ();
}

void
Folded_repeat_iterator::construct_children ()
{
  Repeated_music  *  mus = dynamic_cast<Repeated_music*> (music_l ());
  main_iter_p_ = get_iterator_p (mus->body ());
  if (!main_iter_p_->ok ())
    {
     leave_body ();
      enter_alternative ();
    }
}

void
Folded_repeat_iterator::process (Moment m)
{
  if (!m.to_bool () )
    {
      bool success = try_music (music_l ());
      if (!success)
	music_l ()->origin ()->warning (_ ("no one to print a repeat brace"));
    }
  
  if (main_iter_p_)
    {
      main_iter_p_->process (m);
      if (!main_iter_p_->ok ())
	leave_body ();
    }

  if (!main_iter_p_ && !alternative_iter_p_)
    {
      enter_alternative ();
    }
  
  if (alternative_iter_p_)
    {
      alternative_iter_p_->process (m - main_length_mom_);
      if (!alternative_iter_p_->ok ())
	{
	  delete alternative_iter_p_;
	  alternative_iter_p_ =0;
	}
    }
}

void
Folded_repeat_iterator::leave_body ()
{
  Repeated_music *  mus = dynamic_cast<Repeated_music *> (music_l ());
  delete main_iter_p_;
  main_iter_p_ = 0;
  main_length_mom_ +=  mus->body ()->length_mom ();
}

void
Folded_repeat_iterator::enter_alternative ()
{
  Repeated_music *  mus = dynamic_cast<Repeated_music *> (music_l ());  
  if (mus->alternatives ())
    {
      Simultaneous_music_iterator * s = new Simultaneous_music_iterator;
      s->separate_contexts_b_ = true;
      s->init_translator (mus, report_to_l ());
      
      alternative_iter_p_ = s;
      alternative_iter_p_->construct_children ();
    }
}


Music_iterator*
Folded_repeat_iterator::try_music_in_children (Music * m) const
{
  if (main_iter_p_)
    {
      return main_iter_p_->try_music (m);
    }
  if (alternative_iter_p_)
    return alternative_iter_p_->try_music (m);
  return 0;
}

IMPLEMENT_CTOR_CALLBACK (Folded_repeat_iterator);
