/*   
  percent-repeat-iterator.cc --  implement Percent_repeat_iterator
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "percent-repeat-iterator.hh"
#include "repeated-music.hh"
#include "input.hh"

IMPLEMENT_CTOR_CALLBACK (Percent_repeat_iterator);


Percent_repeat_iterator::Percent_repeat_iterator ()
{
  
}

Percent_repeat_iterator::Percent_repeat_iterator (Percent_repeat_iterator const & p)
  : Music_iterator (p)
{
  child_iter_p_ = p.child_iter_p_ ? p.child_iter_p_->clone (): 0;
  finish_mom_ = p.finish_mom_ ;
}

bool
Percent_repeat_iterator::ok () const
{
  return child_iter_p_;
}

void
Percent_repeat_iterator::construct_children ()
{
  Repeated_music * mus =dynamic_cast<Repeated_music *> (music_l_);
  finish_mom_ = mus->length_mom ();
  child_iter_p_ = get_iterator_p (mus->body ());
}


void
Percent_repeat_iterator::process (Moment m)
{
  if (!m)
    {
      Music_iterator *yeah = try_music (music_l_);
      if (yeah)
	set_translator (yeah->report_to_l ());
      else
	music_l_->origin ()->warning ( _ ("no one to print a percent"));
    }
  
  if (child_iter_p_->ok ())
    child_iter_p_->process (m);

  if (finish_mom_ <= m )
    {
      delete child_iter_p_;
      child_iter_p_ = 0;
    }
}

Moment
Percent_repeat_iterator::pending_moment ()const
{
  if (child_iter_p_->ok ())
    return child_iter_p_->pending_moment ();
  else
    return finish_mom_ ;
}

Music_iterator*
Percent_repeat_iterator::try_music_in_children (Music *m) const
{
  return child_iter_p_->try_music (m);
}


Percent_repeat_iterator::~Percent_repeat_iterator ()
{
  delete child_iter_p_;
}
