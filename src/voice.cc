#include "debug.hh"
#include "voice.hh"

Voice::Voice(Voice const&src)
{
    PL_copy(elts, src.elts);
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
    for (PCursor<Voice_element*> vec(elts); vec.ok(); vec++)
	vec->print();
#endif
}

Real
Voice::last() const
{
    Real l =start;
    for (PCursor<Voice_element*> vec(elts); vec.ok(); vec++)
	l  += vec->duration;
    return l;
}
/****************************************************************/
void
Voice_element::print() const
{
#ifndef NPRINT
    mtor << "voice_element { dur :"<< duration <<"\n";
    for (PCursor<Request*> rc(reqs); rc.ok(); rc++) {
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
    r->elt = this;
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
    duration=src.duration;
    voice_=src.voice_;
    IPointerList__copy(Request*, reqs, src.reqs, clone());
    group=src.group;
    assert(!granted_items.size() && !granted_spanners.size());
}
