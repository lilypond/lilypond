#include "request.hh"
Request::Request(Voice_element*v)
{
    elt = v;
    tag = UNKNOWN;
}

Note_req::Note_req(Voice_element*v):
    Request(v)
{
    name = 'c';
    octave = 0;
    accidental = 0;
    forceacc = false;
    balltype = 1;
    dots = 0;
    tag = NOTE;
}

Rest_req::Rest_req(Voice_element*v)
     :Request(v)
{
    balltype = 1;
    dots = 0;
    tag =REST;    
}

Request::Request()
{
    elt = 0;
    tag = UNKNOWN;    
}

Note_req*
Request::note()
{
    assert(tag == NOTE);
    return (Note_req*)this;
}

Rest_req*
Request::rest()
{
    assert(tag == REST);
    return (Rest_req*)this;
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
Note_req::duration() const {    
    return wholes( balltype,dots);
}
Real
Rest_req::duration() const{
    return wholes( balltype,dots);
}

