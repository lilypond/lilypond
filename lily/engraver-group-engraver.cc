/*
  engravergroup.cc -- implement Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "flower-proto.hh"
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
  for (SCM p = trans_group_list_; gh_pair_p (p); p =gh_cdr ( p))
    {
      Translator * t = unsmob_translator (gh_car (p));
      dynamic_cast<Engraver_group_engraver*> (t)->do_announces ();
    }
  
  while (announce_info_arr_.size ())
    {
      for (int j =0; j < announce_info_arr_.size(); j++)
	{
	  Score_element_info info = announce_info_arr_[j];
	  
	  for (SCM p = simple_trans_list_; gh_pair_p (p); p = gh_cdr (p))
	    {
	      Translator * t = unsmob_translator (gh_car (p));
	      Engraver * eng = dynamic_cast<Engraver*> (t);
	      if (eng && eng!= info.origin_trans_l_)
		eng->acknowledge_element (info);
	    }
	}
      announce_info_arr_.clear ();
      for (SCM p = simple_trans_list_; gh_pair_p (p); p = gh_cdr ( p))
	{
	  Translator * t = unsmob_translator (gh_car (p));
	  Engraver * eng = dynamic_cast<Engraver*> (t);
	  if (eng)
	    eng->process_acknowledged ();
	}
    }
}






