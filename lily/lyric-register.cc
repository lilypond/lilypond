/*
  lyric-register.cc -- implement Lyric_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "staff-walker.hh"
#include "lyric-register.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"

Lyric_register::Lyric_register()
{
}

bool
Lyric_register::try_request(Request*r)
{
    Musical_req * m =r->musical();
    if (!m || ! m->lreq_l()) 
	return false;
    lreq_arr_.push(m->lreq_l());

    return true;
}

void
Lyric_register::process_requests()
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
    get_staff_info().walk_l_->allow_break();
}

void
Lyric_register::post_move_processing()
{
    lreq_arr_.set_size(0);
}


bool
Lyric_register::acceptable_request_b(Request*r)
{
    Musical_req * m =r->musical();
    return (!m || ! m->lreq_l()) ;
}

IMPLEMENT_STATIC_NAME(Lyric_register);
ADD_THIS_REGISTER(Lyric_register);
