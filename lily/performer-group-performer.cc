/*
  performer-group-performer.cc -- implement Performer_group_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Link_array<Translator_group> groups = group_l_arr ();
  for (int i=0; i < groups.size(); i++) 
    {
      Performer_group_performer * group = dynamic_cast<Performer_group_performer*> (groups[i]);
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
	  Audio_element_info info = announce_info_arr_[j];
	  
	  if (!info.req_l_)
	    info.req_l_ = &dummy_req;
	  for (int i=0; i < nongroups.size(); i++) 
	    {	// Is this good enough?
	      Performer * eng = dynamic_cast<Performer*> (nongroups[i]);
	      if (eng && eng!= info.origin_trans_l_arr_[0])
		eng->acknowledge_element (info);
	    }
	}
      announce_info_arr_.clear ();
      for (int i=0; i < nongroups.size(); i++)
	{
	  Performer * eng = dynamic_cast<Performer*> (nongroups[i]);
	  if (eng)
	    eng->process_acknowledged ();
	}

    }
}

