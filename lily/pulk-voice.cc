/*
  pulk-voices.cc -- implement Pulk_voice

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "pulk-voice.hh"
#include "voice.hh"
#include "musical-request.hh"
#include "voice-element.hh"

Pulk_voice::Pulk_voice(Voice*voice_l, int idx)
    :    cur_(voice_l->elts_)
{
    elt_mom_ = voice_l->start_;
    staff_idx_= idx;
    set_subtle();
}

Array<Request*>
Pulk_voice::get_req_l_arr() 
{
    Array<Request*> req_l_arr;
    Moment w = when();
    do {
	Moment sub = subtle_moment_priorities_[subtle_idx_];
	for (PCursor<Request*> i(cur_->req_p_list_); i.ok(); i++) {
	    Musical_req* m_l = i->musical();
	    if (!sub) {
		if (!(m_l && m_l->subtle() && m_l->subtle()->subtime_ )) 
		    req_l_arr.push(i);
	    } else {
		if (m_l && m_l->subtle() && m_l->subtle()->subtime_ == sub)
		    req_l_arr.push(i);
	    }
	}
	next();
    } while ( ok() && when () == w);
    return req_l_arr;
}

void
Pulk_voice::set_subtle()
{
    subtle_idx_ =0;

    subtle_moment_priorities_.set_size(0);
    if (!cur_.ok())
	return;
    for (PCursor<Request*> i(cur_->req_p_list_); i.ok(); i++) {
	Musical_req* m_l = i->musical();
	if (m_l&&m_l->subtle()){
	    Moment sub = m_l->subtle()->subtime_;
	    subtle_moment_priorities_.insert(sub);
	} else {
	    subtle_moment_priorities_.insert(0);
	}
    }
}

void
Pulk_voice::next()
{
    assert(ok());
    subtle_idx_++;
    if (subtle_idx_ == subtle_moment_priorities_.size()) {
	elt_mom_ += cur_->duration_;
	cur_ ++;
	set_subtle();
    }
}

Moment
Pulk_voice::when()const
{
    return elt_mom_ + subtle_moment_priorities_[subtle_idx_];
}
