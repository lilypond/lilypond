#include "debug.hh"
#include "voice.hh"

void
Voice_element::add(Request*r)
{
    if (r->tag == Request::NOTE ||r->tag == Request::REST) {
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
    mtor << "start: "<< start<<eol;
    for (PCursor<Voice_element*> vec(elts); vec.ok(); vec++)
	vec->print();
}
void
Voice_element::print() const
{
    mtor << "voice_element { dur :"<< duration <<"\n";
    for (PCursor<Request*> rc(reqs); rc.ok(); rc++) {
	mtor << "reqtag: " << rc->tag<<eol;
    }
    mtor << "}\n";
}

Mtime
Voice::last() const
{
    Mtime l =start;
    for (PCursor<Voice_element*> vec(elts); vec.ok(); vec++)
	l  += vec->duration;
    return l;
}
