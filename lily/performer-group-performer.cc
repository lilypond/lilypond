/*
  performer-group-performer.cc -- implement Performer_group_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
               Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "performer-group-performer.hh"

#include "debug.hh"


ADD_THIS_TRANSLATOR(Performer_group_performer);


void
Performer_group_performer::announce_element (Audio_element_info info)
{
  announce_info_arr_.push (info);
  Performer::announce_element (info);
}



void
Performer_group_performer::do_announces()
{
  for (SCM p = trans_group_list_; gh_pair_p (p); p =gh_cdr ( p))
    {
      Translator * t = unsmob_translator (gh_car (p));
      dynamic_cast<Performer_group_performer*> (t)->do_announces ();
    }
  
  while (announce_info_arr_.size ())
    {
      for (int j =0; j < announce_info_arr_.size(); j++)
	{
	  Audio_element_info info = announce_info_arr_[j];
	  
	  for (SCM p = simple_trans_list_; gh_pair_p (p); p = gh_cdr (p))
	    {
	      Translator * t = unsmob_translator (gh_car (p));
	      Performer * eng = dynamic_cast<Performer*> (t);
	      if (eng && eng!= info.origin_trans_l_)
		eng->acknowledge_element (info);
	    }
	}
      announce_info_arr_.clear ();
      for (SCM p = simple_trans_list_; gh_pair_p (p); p = gh_cdr ( p))
	{
	  Translator * t = unsmob_translator (gh_car (p));
	  Performer * eng = dynamic_cast<Performer*> (t);
	  if (eng)
	    eng->process_acknowledged ();
	}
    }
}






