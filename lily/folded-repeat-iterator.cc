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
  main_iter_ = 0;
  alternative_iter_ = 0;
}

bool
Folded_repeat_iterator::ok () const
{
  return main_iter_ || alternative_iter_;
}

Folded_repeat_iterator::~Folded_repeat_iterator ()
{
  delete main_iter_;
  delete alternative_iter_;
}

Folded_repeat_iterator::Folded_repeat_iterator (Folded_repeat_iterator const &src)
  : Music_iterator (src)
{
  main_iter_ = src.main_iter_ ? src.main_iter_->clone () : 0;
  alternative_iter_ = src.alternative_iter_ ? src.alternative_iter_->clone () : 0;
  main_length_mom_ = src.main_length_mom_;
}

Moment
Folded_repeat_iterator::pending_moment () const
{
  if (main_iter_)
    {
      return main_iter_->pending_moment ();
    }
  else
    return main_length_mom_ + alternative_iter_->pending_moment ();
}

void
Folded_repeat_iterator::construct_children ()
{
  Repeated_music  *  mus = dynamic_cast<Repeated_music*> (get_music ());
  main_iter_ = get_iterator (mus->body ());
  if (!main_iter_->ok ())
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
      bool success = try_music (get_music ());
      if (!success)
	get_music ()->origin ()->warning (_ ("no one to print a repeat brace"));
    }
  
  if (main_iter_)
    {
      main_iter_->process (m);
      if (!main_iter_->ok ())
	leave_body ();
    }

  if (!main_iter_ && !alternative_iter_)
    {
      enter_alternative ();
    }
  
  if (alternative_iter_)
    {
      alternative_iter_->process (m - main_length_mom_);
      if (!alternative_iter_->ok ())
	{
	  delete alternative_iter_;
	  alternative_iter_ =0;
	}
    }
}

void
Folded_repeat_iterator::leave_body ()
{
  Repeated_music *  mus = dynamic_cast<Repeated_music *> (get_music ());
  delete main_iter_;
  main_iter_ = 0;
  main_length_mom_ +=  mus->body ()->length_mom ();
}

void
Folded_repeat_iterator::enter_alternative ()
{
  Repeated_music *  mus = dynamic_cast<Repeated_music *> (get_music ());  
  if (mus->alternatives ())
    {
      Simultaneous_music_iterator * s = new Simultaneous_music_iterator;
      s->separate_contexts_b_ = true;
      s->init_translator (mus, report_to ());
      
      alternative_iter_ = s;
      alternative_iter_->construct_children ();
    }
}


Music_iterator*
Folded_repeat_iterator::try_music_in_children (Music * m) const
{
  if (main_iter_)
    {
      return main_iter_->try_music (m);
    }
  if (alternative_iter_)
    return alternative_iter_->try_music (m);
  return 0;
}

IMPLEMENT_CTOR_CALLBACK (Folded_repeat_iterator);
