#include "debug.hh"
#include "voice.hh"
#include "request.hh"

void
Voice::set_default_group(String s)
{
    elts.top()->set_default_group(s);
}

Voice::Voice(Voice const&src)
{
    for (iter_top(src.elts, i); i.ok(); i++)
	add(new Voice_element(**i));

    start = src.start;
}

Voice::Voice()
{
    start = 0.0;
}

void
Voice::add(Voice_element*v)
{
    v->voice_l_ = this;
    elts.bottom().add(v);
}

void
Voice::print() const
{
#ifndef NPRINT
    mtor << "start: "<< start<<eol;
    for (iter_top(elts,i); i.ok(); i++)
	i->print();
#endif
}

Moment
Voice::last() const
{
    Moment l =0;
    if (elts.size())
	l = start;
    
    for (iter_top(elts,i); i.ok(); i++)
	l  += i->duration;
    return l;
}
/****************************************************************/
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
    if (r->rhythmic()) {
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
    defined_ch_c_l_m = 0;
}

Voice_element::Voice_element(Voice_element const&src)
{
    defined_ch_c_l_m = src.defined_ch_c_l_m;
		// are you sure? They can be modified after copying.
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
