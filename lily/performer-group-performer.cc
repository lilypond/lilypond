/*
  performer-group-performer.cc -- implement Performer_group_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
               Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "performer-group-performer.hh"

#include "audio-element.hh"
#include "warn.hh"


ENTER_DESCRIPTION(Performer_group_performer,
/* descr */       "",
/* creats*/       "",
/* acks  */       "",
/* reads */       "",
/* write */       "");



void
Performer_group_performer::announce_element (Audio_element_info info)
{
  announce_infos_.push (info);
  Performer::announce_element (info);
}



void
Performer_group_performer::create_audio_elements ()
{
  for (SCM p = simple_trans_list_; gh_pair_p (p); p = ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
      Performer * eng = dynamic_cast<Performer*> (t);
      if (eng)
	eng->create_audio_elements ();
    }
}

void
Performer_group_performer::acknowledge_audio_elements ()
{
  for (int j =0; j < announce_infos_.size (); j++)
    {
      Audio_element_info info = announce_infos_[j];

      for (SCM p = simple_trans_list_; gh_pair_p (p); p = ly_cdr (p))
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
  for (SCM p = trans_group_list_; gh_pair_p (p); p =ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
      dynamic_cast<Performer_group_performer*> (t)->do_announces ();
    }

  
  create_audio_elements ();
    
  while (announce_infos_.size ())
    {
      acknowledge_audio_elements ();
      announce_infos_.clear ();
      create_audio_elements ();
    }

  if (announce_infos_.size ())
    {
#if 0  //printf?  -> include <stdio.h>     
      printf ("do_announces: elt: %s\n",
	      classname (announce_infos_[0].elem_));
#endif      
      announce_infos_.clear ();
    }
}

Performer_group_performer::Performer_group_performer(){}
