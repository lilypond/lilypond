/*
  engravergroup.cc -- implement Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "engraver-group.hh"
#include "engraver.hh"
#include "debug.hh"
#include "p-score.hh"
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
  Link_array<Translator_group> groups = group_l_arr ();
  for (int i=0; i < groups.size(); i++) 
    {
      Engraver_group_engraver * group = dynamic_cast<Engraver_group_engraver*> (groups[i]);
      if (group)
	{
	  group->do_announces();
	}
    }
  
  Request dummy_req;

  Link_array<Translator> nongroups = nongroup_l_arr ();
  while (announce_info_arr_.size ())
    {
      for (int j =0; j < announce_info_arr_.size(); j++)
	{
	  Score_element_info info = announce_info_arr_[j];
	  
	  if (!info.req_l_)
	    info.req_l_ = &dummy_req;
	  for (int i=0; i < nongroups.size(); i++) 
	    {	// Is this good enough?
	      Engraver * eng = dynamic_cast<Engraver*> (nongroups[i]);
	      if (eng && eng!= info.origin_trans_l_arr_[0])
		eng->acknowledge_element (info);
	    }
	}
      announce_info_arr_.clear ();
      for (int i=0; i < nongroups.size(); i++)
	{
	  Engraver * eng = dynamic_cast<Engraver*> (nongroups[i]);
	  if (eng)
	    eng->process_acknowledged ();
	}

    }
}


Staff_info
Engraver_group_engraver::get_staff_info() const
{
  Staff_info inf = Engraver::get_staff_info();

  Link_array<Translator> simple_translators = nongroup_l_arr (); 
  for (int i=0; i < simple_translators.size(); i++)
    {
    Engraver * eng = dynamic_cast<Engraver*> (simple_translators[i]);
    if (eng)
      eng->fill_staff_info (inf);
    }
  return inf;
}



