/*
  lyric-performer.cc -- implement Lyric_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
           Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "lyric-performer.hh"
#include "musical-request.hh"
//#include "text-item.hh"
//#include "paper-def.hh"
//#include "lookup.hh"


IMPLEMENT_STATIC_NAME(Lyric_performer);
IMPLEMENT_IS_TYPE_B1(Lyric_performer,Performer);
ADD_THIS_PERFORMER(Lyric_performer);

Lyric_performer::Lyric_performer()
{
}

Lyric_performer::~Lyric_performer()
{
}

bool
Lyric_performer::do_try_request( Request* req_l )
{
    Musical_req* m_l = req_l->musical();
    if ( !m_l || ! m_l->lreq_l() ) 
	return false;
    lreq_arr_.push( m_l->lreq_l() );

    return true;
}

void
Lyric_performer::do_process_requests()
{
#if 0
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
#endif
}

void
Lyric_performer::do_post_move_processing()
{
    lreq_arr_.set_size(0);
}

