/*
  voiceelt.cc -- implement Voice_element

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "debug.hh"
#include "voice.hh"
#include "voice-element.hh"
#include "musicalrequest.hh"
#include "commandrequest.hh"


void
Voice_element::print() const
{
#ifndef NPRINT
    mtor << "voice_element { dur :"<< duration <<"\n";
    for (iter_top(reqs,rc); rc.ok(); rc++) {
	rc->print();
    }
    mtor << "}\n";
#endif
}

void
Voice_element::add(Request*r)
{
    if (r->duration()) {
	assert (!duration  || duration == r->duration());
	duration = r->duration();
    }
    
    r->elt_l_ = this;
    reqs.bottom().add(r);
}


Voice_element::Voice_element()
{
    voice_l_ = 0;
    duration = 0;
    defined_ch_c_l_ = 0;
}

Voice_element::Voice_element(Voice_element const&src)
{
    defined_ch_c_l_ = src.defined_ch_c_l_;

    voice_l_=0;
    for (iter_top(src.reqs, i); i.ok(); i++)
	add(i->clone());

}
bool
Voice_element::find_plet_start_bo(char c, Moment& moment_r)
{
    assert( c == ']' );
    moment_r += duration;
    for ( PCursor<Request*> req_l_pcur( reqs.top() ); req_l_pcur.ok(); req_l_pcur++ ) {
	if (req_l_pcur->beam() && req_l_pcur->beam()->spantype == Span_req::START )
	    return true;
    }
    return false;
}

void
Voice_element::set_default_group(String s)
{
    for (iter_top(reqs, i); i.ok(); i++)
	if (i->groupchange())
	    return ;
    Group_change_req *greq = new Group_change_req;
    greq->newgroup_str_ = s;
    add(greq);
}

void
Voice_element::set_plet_backwards(Moment& now_moment_r,
				  Moment until_moment, int num_i, int den_i)
{
    now_moment_r += duration;
    if ( now_moment_r > until_moment )
    	return;
    for ( PCursor<Request*> req_l_pcur( reqs.top() ); req_l_pcur.ok(); req_l_pcur++ ) {
	if (req_l_pcur->beam() && req_l_pcur->beam()->spantype == Span_req::START )
	    req_l_pcur->beam()->nplet = den_i;
	if (req_l_pcur->rhythmic())
	    req_l_pcur->rhythmic()->plet_factor = Moment(num_i, den_i);
	if (req_l_pcur->stem())
	    req_l_pcur->stem()->plet_factor = Moment(num_i, den_i);
    }
}
