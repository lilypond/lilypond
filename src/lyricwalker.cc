/*
  lyricwalker.cc -- implement Lyric_walker

  source file of the LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "musicalrequest.hh"
#include "voice.hh"
#include "pscore.hh"
#include "lyricstaff.hh"
#include "lyricwalker.hh"
#include "debug.hh"
#include "lyricitem.hh"

void
Lyric_walker::process_requests()
{
    allow_break();
    for (int i = 0; i <  lcol_l()->lreq_l_array_.size(); i++)  {
	lcol_l()->typeset_item(
	    new Lyric_item(lcol_l()->lreq_l_array_[i],i)
	    );
    }
}

Lyric_walker::Lyric_walker(Lyric_staff* lstaff_l)
    : Staff_walker(lstaff_l, lstaff_l->pstaff_l_->pscore_l_)
{

}



Lyric_staff*
Lyric_walker::lstaff_l()
{
    return (Lyric_staff*)staff_l_;
}

Lyric_column*
Lyric_walker::lcol_l()
{
    return (Lyric_column*) *(*this);
}
