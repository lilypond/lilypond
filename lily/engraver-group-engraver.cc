/*
  engravergroup.cc -- implement Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "engraver-group-engraver.hh"
#include "engraver.hh"
#include "debug.hh"
#include "paper-score.hh"
#include "score-element.hh"


ADD_THIS_TRANSLATOR(Engraver_group_engraver);

void
Engraver_group_engraver::announce_element (Score_element_info info)
{
  announce_info_arr_.push (info);
  Engraver::announce_element (info);
}

void
Engraver_group_engraver::do_announces()
{
  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
    {
      if (Engraver_group_engraver *trg =  dynamic_cast <Engraver_group_engraver *> (p->car_))
	trg->do_announces ();
    }

  Request dummy_req;

  while (announce_info_arr_.size ())
    {
      for (int j =0; j < announce_info_arr_.size(); j++)
	{
	  Score_element_info info = announce_info_arr_[j];
	  
	  if (!info.req_l_)
	    info.req_l_ = &dummy_req;

	  for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
	    {
	      if (!dynamic_cast <Engraver_group_engraver *> (p->car_))
		{
		  Engraver * eng = dynamic_cast<Engraver*> (p->car_);
		  if (eng && eng!= info.origin_trans_l_arr_[0])
		    eng->acknowledge_element (info);
		}		  
	    }
	}
      
      announce_info_arr_.clear ();
      for (Cons<Translator> *p = trans_p_list_.head_; p; p = p->next_)
	{
	  if (!dynamic_cast <Engraver_group_engraver *> (p->car_))
	    {
	      Engraver * eng = dynamic_cast<Engraver*> (p->car_);
	      if (eng)
		eng->process_acknowledged ();
	    }
	}
    }
}


Staff_info
Engraver_group_engraver::get_staff_info() const
{
  Staff_info inf = Engraver::get_staff_info();
  return inf;
}



