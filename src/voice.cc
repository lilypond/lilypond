#include "debug.hh"
#include "voice.hh"
#include "request.hh"

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
    v->voice_ = this;
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
	assert (!duration);	    
	duration = r->duration();
    }
    r->elt_l_ = this;
    reqs.bottom().add(r);
}


Voice_element::Voice_element()
{
    voice_ = 0;
    group = 0;
    duration = 0.0;
}

Voice_element::Voice_element(Voice_element const&src)
{
    voice_=src.voice_;
    for (iter_top(src.reqs, i); i.ok(); i++)
	add(i->clone());
    group=src.group;
}
