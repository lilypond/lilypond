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
  iter_p_ = src.iter_p_->clone ();
}

Sequential_music_iterator::~Sequential_music_iterator()
{
  if (iter_p_)
    {
      /*      if (iter_p_->ok () )
	music_l_->origin ()->warning (_ ("Must stop before this music ends"));
      */
      delete iter_p_;
      iter_p_ = 0;
    }
}


void
Sequential_music_iterator::construct_children()
{
  cursor_ = dynamic_cast<Music_sequence const*> (music_l_)->music_list ();
  
  while (gh_pair_p (cursor_ ))
    {
      start_next_element();
      if (!iter_p_->ok()) 
	{
	  leave_element();
	}
      else 
	{
	  set_sequential_music_translator();
	  break;
	}
    }
}

void 
Sequential_music_iterator::leave_element()
{
  delete iter_p_;
  iter_p_ =0;
  Moment elt_time = unsmob_music (gh_car (cursor_))->length_mom ();
  here_mom_ += elt_time;
  cursor_ =gh_cdr (cursor_);
}

void
Sequential_music_iterator::start_next_element()
{
  assert (!iter_p_);
  iter_p_ = get_iterator_p (unsmob_music (gh_car (cursor_)));
}

void
Sequential_music_iterator::set_sequential_music_translator()
{
  Translator_group  * child_report = child_report = iter_p_->report_to_l ();
  if (dynamic_cast<Grace_iterator*> (iter_p_))
    child_report = child_report->daddy_trans_l_;
    
  if (report_to_l()->depth_i () < child_report->depth_i ())
    set_translator (child_report);
}

/*
  [todo: translate]
  
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
 
void
Sequential_music_iterator::process (Moment until)
{
  if (ok ())
    {
      while (1) 
	{
	  Moment local_until = until - here_mom_;
	  while (iter_p_->ok ()) 
	    {
	      Moment here = iter_p_->pending_moment ();
	      if (here != local_until)
		return ;
	      
	      iter_p_->process (local_until);
	    }
	  
	  if (!iter_p_->ok ()) 
	    {
	      set_sequential_music_translator ();
	      leave_element ();
	      
	      if (gh_pair_p (cursor_))
		start_next_element ();
	      else 
		return ;
	    }
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
