/*
  voice-elt.cc -- implement Voice_element

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "debug.hh"
#include "voice.hh"
#include "voice-element.hh"
#include "musical-request.hh"
#include "command-request.hh"


void
Voice_element::transpose(Melodic_req const&d)const
{
    for (iter_top(req_p_list_,i); i.ok(); i++) {
	i->transpose(d);
      }
}

void
Voice_element::print() const
{
#ifndef NPRINT
    mtor << "voice_element { dur :"<< duration_ <<"\n";
    for (iter_top(req_p_list_,rc); rc.ok(); rc++) {
	rc->print();
    }
    mtor << "}\n";
#endif
}

void
Voice_element::add(Request*r)
{
    if (r->duration()) {
	assert (!duration_  || duration_ == r->duration());
	duration_ = r->duration();
    }
    
    r->elt_l_ = this;
    req_p_list_.bottom().add(r);
}


Voice_element::Voice_element()
{
    voice_C_ = 0;
    duration_ = 0;
}

Voice_element::Voice_element(Voice_element const&src)
    : Input(src)
{
  
    voice_C_=0;
    for (iter_top(src.req_p_list_, i); i.ok(); i++)
	add(i->clone());

}
bool
Voice_element::find_plet_start_b(char c, Moment& moment_r)// b unused?
{
    assert( c == ']' );
    moment_r += duration_;
    for ( PCursor<Request*> i( req_p_list_.top() ); i.ok(); i++ ) {
	if (i->beam() && i->beam()->spantype == Span_req::START )
	    return true;
    }
    return false;
}

void
Voice_element::set_default_group(String s)
{
    for (iter_top(req_p_list_, i); i.ok(); i++)
	if (i->command() &&i->command()->groupchange())
	    return ;
    Group_change_req *greq = new Group_change_req;
    greq->newgroup_str_ = s;
    add(greq);
}

void
Voice_element::set_plet_backwards(Moment& now_moment_r,
				  Moment until_moment, int num_i, int den_i)
{
    now_moment_r += duration_;
    if ( now_moment_r > until_moment )
    	return;
    for ( PCursor<Request*> i( req_p_list_.top() ); i.ok(); i++ ) {
	if (i->beam() && i->beam()->spantype == Span_req::START )
	    i->beam()->nplet = den_i;
	if (i->rhythmic()) {
	    i->rhythmic()->duration_.plet_.type_i_ = den_i;
	    i->rhythmic()->duration_.plet_.iso_i_  = num_i;
	    
	}
    }
}
