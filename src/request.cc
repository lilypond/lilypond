#include "request.hh"
#include "misc.hh"
#include "debug.hh"

#define VIRTUALCONS(T,R) R *T::clone() const { return  new T(*this); } struct T
#define RCONS(T) VIRTUALCONS(T, Request)

RCONS(Rest_req);
RCONS(Rhythmic_req);
RCONS(Stem_req);
RCONS(Note_req);
RCONS(Span_req);
RCONS(Slur_req);
RCONS(Beam_req);

void
Request::print() const    
{
#ifndef NPRINT
    mtor << "Req{ unknown }\n";
#endif
}

Request::Request()
{
    elt = 0;
}

Note_req::Note_req()
{
    name = 0;
    octave = 0;
    accidental = 0;
    forceacc = false;
}

int
Note_req::height() const
{
    return  name + octave*7;
}

Rhythmic_req::Rhythmic_req()
{
    balltype = 1;
    dots = 0;
}

void
Rhythmic_req::print() const
{
    mtor << "rhythmic: " << balltype ;
    int d =dots;
    while (d--)
	mtor << '.';
    mtor<<"\n";
}

void
Note_req::print() const
{
    mtor << "note: " << name << " oct: "<< octave;
    Rhythmic_req::print();
}

void
Rest_req::print() const
{
    mtor << "rest, " ;
    Rhythmic_req::print();
}


Real
Rhythmic_req::duration() const {    
    return wholes( balltype,dots);
}

Beam_req::Beam_req()
{
    nplet = 0;
}

Span_req::Span_req()
{
    spantype = NOSPAN;
}
