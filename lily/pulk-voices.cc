/*
  pulk-voices.cc -- implement Pulk_voices

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "pulk-voice.hh"
#include "pulk-voices.hh"
#include "staff.hh"
#include "voice.hh"
#include "request-column.hh"
#include "debug.hh"

Pulk_voices::Pulk_voices(Link_list<Staff*> const& l)
    : staff_l_list_(l)
{
    int staff_i = 0;
    last_= 0;
    Moment min_staff_last_mom=1e8; // ugh
    for (iter_top(l, i); i.ok(); i++,	staff_i++) {
	Moment staff_last=0;
	for (iter_top(i->voice_list_,j); j.ok(); j++) {
	    if (j->elts_.size()) {
		staff_last = staff_last >? j->last();
		voice_pq_.insert(Voice_l(j, staff_i));
	    }
	}
	min_staff_last_mom = min_staff_last_mom <? staff_last;
	last_ = last_ >? staff_last;
    }
    next_mom_ = voice_pq_.front().l_->start_;
    time_arr_.set_size(staff_i);

    if (last_ != min_staff_last_mom)
	warning("Not all staffs end simultaneously");

}

void
Pulk_voices::get_aligned_request(Request_column* col_l)
{
    while (voice_pq_.size() && voice_pq_.front().l_->start_ == next_mom_) {
	Voice_l v = voice_pq_.get();
	pulk_p_list_.bottom().add( new Pulk_voice ( v.l_, v.staff_idx_ ));
    }

    /* hairy. #i# is a cursor which points to Pulk_voice (which a
       cursor, essentially) 
       
       
       At the same time bookkeeping of measures is done.
       */
    
    Moment new_next_mom = last_;
    iter(pulk_p_list_.top() , i);
    while  ( i.ok() ) {
	mtor << "considering staff"<< i->staff_idx_ << "at " << i->when() << "\n";
	assert (i->when() >= next_mom_);
	if (i->when() == next_mom_) {
	    col_l->add_reqs(i->staff_idx_, i->get_req_l_arr() );
	    Time_description &t_r  = time_arr_[i->staff_idx_]; 
	    col_l->update_time(i->staff_idx_, t_r);
	    
	    if (! t_r.cadenza_b_ && t_r.next_bar_moment() > next_mom_) {
		mtor << "next bar at: " << t_r.next_bar_moment();
		new_next_mom = new_next_mom <? t_r.next_bar_moment();
	    }

	    if (!i->ok()) {
		i.del();
		continue;
	    }
	}
	assert( i->when()  > next_mom_);
	/* no need to call i->next(), since this is done automatically */
	new_next_mom = new_next_mom <? i->when();

	i++;

    }
    
    if (voice_pq_.size() )
	new_next_mom = new_next_mom <? voice_pq_.front().l_->start_;

    for (int j=0; j < time_arr_.size(); j++)
	time_arr_[j].add( new_next_mom - next_mom_  );

    if (pulk_p_list_.size())
	assert( new_next_mom > next_mom_);
    next_mom_ = new_next_mom;
}


bool
Pulk_voices::ok() const
{
    return pulk_p_list_.size() || voice_pq_.size();
}

int
compare(Voice_l const& v1, Voice_l const& v2)
{
    return sign(v1.l_->start_ - v2.l_->start_);
}
	    
Moment
Pulk_voices::next_mom()const
{
    return next_mom_;
}

bool
Pulk_voices::time_checks_failed_b()const
{
    bool b = false;
    for (int i=0; !b && i < time_arr_.size(); i++)
	b|=time_arr_[i].error_b_;
    return b;
}
