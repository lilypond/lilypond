/*
  voiceelt.cc -- implement Voice_element

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "voice.hh"
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
