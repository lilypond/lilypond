#include "request.hh"
#include "debug.hh"

void
Request::print() const    
{
#ifndef NPRINT
    mtor << "Req{ unknown }\n";
#endif
}

Request::Request(Voice_element*v)
{
    elt = v;
}

Note_req::Note_req(Voice_element*v)
    : Rhythmic_req(v)
{
    name = 'c';
    octave = 0;
    accidental = 0;
    forceacc = false;
}

Rhythmic_req::Rhythmic_req(Voice_element*v)
     :Request(v)
{
    balltype = 1;
    dots = 0;
}

Request::Request()
{
    elt = 0;
}


Real
wholes(int dur, int dots)
{
    Real f = 1.0/Real(dur);
    Real delta = f;

    while (dots--) {
	delta /= 2.0;
	f += delta;
    }
    return f;    
}

Real
Rhythmic_req::duration() const {    
    return wholes( balltype,dots);
}

