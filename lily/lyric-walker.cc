/*
  lyricwalker.cc -- implement Lyric_walker

  source file of the LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "musical-request.hh"
#include "voice.hh"
#include "p-score.hh"
#include "lyric-staff.hh"
#include "lyric-walker.hh"
#include "debug.hh"
#include "lyric-item.hh"
#include "staff-column.hh"

void
Lyric_walker::process_requests()
{
    allow_break();
    
    int req_count=0;
    for (int i = 0; i < ptr()->musicalreq_l_arr_.size(); i++)  {
	Lyric_req * lreq_l = ptr()->musicalreq_l_arr_[i]->lreq_l();
	if (!lreq_l)
	    continue;
	Item *lp = new Lyric_item(lreq_l,req_count++);
	ptr()->typeset_musical_item( lp);
    }
}

Lyric_walker::Lyric_walker(Lyric_staff* lstaff_l)
    : Staff_walker(lstaff_l, lstaff_l->pstaff_l_->pscore_l_)
{

}


