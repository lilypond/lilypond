/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "lyric-engraver.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"

Lyric_engraver::Lyric_engraver()
{
}

bool
Lyric_engraver::do_try_request(Request*r)
{
    Musical_req * m =r->musical();
    if (!m || ! m->lreq_l()) 
	return false;
    lreq_arr_.push(m->lreq_l());

    return true;
}

void
Lyric_engraver::do_process_requests()
{
    Text_item * last_item_l =0;
    for (int i=0; i < lreq_arr_.size(); i++) {
	Text_item *lp = new Text_item(lreq_arr_[i]->tdef_p_ );
	lp->dir_i_ = -1;
	lp->fat_b_ = true;
	if (last_item_l)
	    lp->add_support(last_item_l);
	last_item_l = lp;
	typeset_element(lp);
    }
}

void
Lyric_engraver::do_post_move_processing()
{
    lreq_arr_.set_size(0);
}


IMPLEMENT_STATIC_NAME(Lyric_engraver);
IMPLEMENT_IS_TYPE_B1(Lyric_engraver,Engraver);
ADD_THIS_ENGRAVER(Lyric_engraver);
