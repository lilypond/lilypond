/*
  performer-group-performer.cc -- implement Performer_group_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
   for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
    {
      if (Performer_group_performer *trg =  dynamic_cast <Performer_group_performer *> (p->car_))
	trg->do_announces ();
    }

  
  Request dummy_req;

  while (announce_info_arr_.size ())
    {
      for (int j =0; j < announce_info_arr_.size(); j++)
	{
	  Audio_element_info info = announce_info_arr_[j];
	  
	  if (!info.req_l_)
	    info.req_l_ = &dummy_req;

	  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
	    {
	      if (!dynamic_cast <Performer_group_performer *> (p->car_))
		{
		  Performer * eng = dynamic_cast<Performer*> (p->car_);
		  // urg, huh? core dump?
		  //if (eng && eng!= info.origin_trans_l_arr_[0])
		  if (eng && info.origin_trans_l_arr_.size ()
		      && eng!= info.origin_trans_l_arr_[0])
		    eng->acknowledge_element (info);
		}
	    }
	  announce_info_arr_.clear ();
      
      
	  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
	    {
	      if (!dynamic_cast <Performer_group_performer *> (p->car_))
		{
		  Performer * eng = dynamic_cast<Performer*> (p->car_);
		  if (eng)
		    eng->process_acknowledged ();
		}
	    }
	}
    }
}

