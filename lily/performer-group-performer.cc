/*
  performer-group-performer.cc -- implement Performer_group_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
               Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "performer-group-performer.hh"
#include "context.hh"
#include "audio-element.hh"
#include "warn.hh"


ENTER_DESCRIPTION(Performer_group_performer,
/* descr */       "",
/* creats*/       "",
/* accepts */     "",
/* acks  */      "",
/* reads */       "",
/* write */       "");



void
Performer_group_performer::announce_element (Audio_element_info info)
{
  announce_infos_.push (info);
  Translator *t
    = unsmob_translator (daddy_context_->daddy_context_->implementation_);

  if (Performer_group_performer * eg = dynamic_cast<Performer_group_performer*> (t))
    eg->announce_element (info);
}

void
Performer_group_performer::acknowledge_audio_elements ()
{
  for (int j =0; j < announce_infos_.size (); j++)
    {
      Audio_element_info info = announce_infos_[j];

      for (SCM p = get_simple_trans_list (); gh_pair_p (p); p = ly_cdr (p))
	{
	  Translator * t = unsmob_translator (ly_car (p));
	  Performer * eng = dynamic_cast<Performer*> (t);
	  if (eng && eng!= info.origin_trans_)
	    eng->acknowledge_audio_element (info);
	}
    }
}

void
Performer_group_performer::do_announces ()
{
  while (1)
    {
      performer_each (get_simple_trans_list (),
		      &Performer::create_audio_elements);
  
      if (!announce_infos_.size ())
	break ;
      
      acknowledge_audio_elements ();
      announce_infos_.clear ();
    }
}

Performer_group_performer::Performer_group_performer()
{
}

/* c&p engraver-group.cc */
void
recurse_down_performers (Context * c, Performer_method ptr, bool context_first)
{
  Performer_group_performer * tg
    = dynamic_cast<Performer_group_performer*> (unsmob_translator (c->implementation_));


  if (!context_first)
    {
      performer_each (tg->get_simple_trans_list (),
		     ptr);

      (tg->*ptr) ();
    }

  for (SCM s = c->context_list_ ; gh_pair_p (s);
       s =gh_cdr (s))
    {
      recurse_down_performers (unsmob_context (gh_car (s)), ptr, context_first);
    }

  if (context_first)
    {
      performer_each (tg->get_simple_trans_list (),
		     ptr);
      (tg->*ptr) ();
    }
}


void
performer_each (SCM list, Performer_method method)
{
  for (SCM p = list; gh_pair_p (p); p = ly_cdr (p))
    {
      Performer * e = dynamic_cast<Performer*>(unsmob_translator (ly_car (p)));
      if (e)
	(e->*method) ();
    }
}
