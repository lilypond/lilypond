/*
  Sequential_music_iterator.cc -- implement Sequential_music_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "grace-iterator.hh"
#include "translator-group.hh"
#include "debug.hh"
#include "sequential-music-iterator.hh"
#include "music-list.hh"
#include "request-chord-iterator.hh"

Sequential_music_iterator::Sequential_music_iterator ()
{
  cursor_ = 0;
  here_mom_ = 0;
  iter_p_ =0;
}

Sequential_music_iterator::Sequential_music_iterator (Sequential_music_iterator const &src)
  : Music_iterator (src)
{
  cursor_ = src.cursor_;
  here_mom_ = src.here_mom_;
  if (src.iter_p_)
    iter_p_ = src.iter_p_->clone ();
  else
    iter_p_ = 0;
}

Sequential_music_iterator::~Sequential_music_iterator()
{
  if (iter_p_)
    {
#if 0
      if (iter_p_->ok () )
	music_l_->origin ()->warning (_ ("Must stop before this music ends"));
#endif
      delete iter_p_;
    }
}

void
Sequential_music_iterator::construct_children()
{
  cursor_ = dynamic_cast<Music_sequence const*> (music_l_)->music_list ();
  
  while (gh_pair_p (cursor_ ))
    {
      iter_p_ =  get_iterator_p (unsmob_music (gh_car (cursor_)));
      
      if (iter_p_->ok()) 
	{
	  descend_to_child ();
	  return;
	}

      delete iter_p_ ;
      iter_p_ =0;
      cursor_ = gh_cdr (cursor_);
    }
}
 /*
  move to context of child iterator if it is deeper down in the
  hierarchy.
  */

void
Sequential_music_iterator::descend_to_child ()
{
       
  Translator_group  * child_report = child_report = iter_p_->report_to_l ();
  if (dynamic_cast<Grace_iterator*> (iter_p_))
    child_report = child_report->daddy_trans_l_;
    
  if (report_to_l()->depth_i () < child_report->depth_i ())
    set_translator (child_report);
}


/*
  
  
  Hier staat in feite: haal alle muziek op (startend op tijd HERE) tot
  je iets met lengte L > 0 tegenkomt.  Aangezien de preconditie is dat
  UNTIL het eerstvolgende event is, weet je (per definitie)

  L >= (UNTIL - HERE)

  en iets wat hierna komt (op tijd T) komt dus na tijd

  HERE + L >= HERE + (UNTIL - HERE) = UNTIL

  Dus als je een L>0 tegenkomt, wil je de rest niet meer. Aangezien
  alles wat tot nu toe hebt gespaard op HERE begint, is dat precies wat
  je nodig hebt.

  Misschien kan je deze comment erbij stoppen, en moeten we de
  eigenschappen van het muziek datatype wat formaliseren, zodat deze
  redenering helderder is.
*/

SCM
Sequential_music_iterator::get_music (Moment until)const
{
  SCM s = SCM_EOL;
  if (until <  pending_moment ())
    return s;

  SCM curs = cursor_;
  Music_iterator * iter = iter_p_->clone ();
  while (1)
    {
      SCM nm = iter->get_music (until - here_mom_);
      s = gh_append2 (nm, s);
      
      Moment m = 0;
      for (SCM i = nm; gh_pair_p(i); i = gh_cdr (i))
	m = m >? unsmob_music (gh_car (i))->length_mom ();

      delete iter;

      curs = gh_cdr (curs);

      if (!gh_pair_p (curs) ||  m > Moment (0))
	return s;
      else
	{
	  iter = get_iterator_p (unsmob_music (gh_car (curs)));
	}      
    }
  return s;
}
/*
  Skip events till UNTIL. We don't do any other side effects (such as
  moving descending to child iterator contexts, because they might
  depend on \context specs and \translator changes being executed
    
 */
void
Sequential_music_iterator::skip (Moment until)
{
  SCM curs = cursor_;
  while (1)
    {
      Moment l =iter_p_->music_length_mom ();
      if (l >= until - here_mom_)
	iter_p_->skip (until - here_mom_);

      if (iter_p_->ok ())
	return ; 
      
      here_mom_ = here_mom_ + l;
      delete iter_p_;
      iter_p_ =0;

      curs = gh_cdr (curs);

      if (!gh_pair_p (curs))
	return ;
      else
	iter_p_ = get_iterator_p (unsmob_music (gh_car (curs)));
    }
}

void
Sequential_music_iterator::process (Moment until)
{
  while (1)
    {
      iter_p_->process (until - here_mom_);

      /*
	if the iter is still OK, there must be events left that have
	
	  TIME > LEFT
	  
      */
      if (iter_p_->ok ())
	return ;

      here_mom_ += iter_p_->music_length_mom ();

      descend_to_child ();
      delete iter_p_;
      iter_p_ =0;

      cursor_ = gh_cdr (cursor_);

      if (!gh_pair_p (cursor_))
	return ;
      else
	{
	  delete iter_p_;
	  iter_p_ = get_iterator_p (unsmob_music (gh_car (cursor_)));
	}      
    }

}

Moment
Sequential_music_iterator::pending_moment() const
{
  return iter_p_->pending_moment() + here_mom_;
}


bool
Sequential_music_iterator::ok() const
{
  return iter_p_;
}

Music_iterator*
Sequential_music_iterator::try_music_in_children (Music *m) const
{ 
  return iter_p_ ? iter_p_->try_music (m) : 0;
}
