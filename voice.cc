#include "debug.hh"
#include "voice.hh"

void
Voice_element::add(Request*r)
{
    if (r->rhythmic()) {
	assert (!duration);	    
	duration = r->duration();
    }
    reqs.bottom().add(r);
}

Voice::Voice()
{
    start = 0.0;
}

void
Voice::add(Voice_element*v)
{
    elts.bottom().add(v);
}

Voice_element::Voice_element()
{
    voice = 0;
    group = 0;
    duration = 0.0;
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

Real
Voice::last() const
{
    Real l =start;
    for (PCursor<Voice_element*> vec(elts); vec.ok(); vec++)
	l  += vec->duration;
    return l;
}
