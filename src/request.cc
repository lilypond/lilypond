#include "request.hh"
#include "misc.hh"
#include "debug.hh"
#include "scriptdef.hh"
#include "textdef.hh"

#define VIRTUALCONS(T,R) R *T::clone() const { return  new T(*this); } struct T
#define RCONS(T) VIRTUALCONS(T, Request)

RCONS(Rest_req);
RCONS(Text_req);
RCONS(Rhythmic_req);
RCONS(Stem_req);
RCONS(Script_req);
RCONS(Note_req);
RCONS(Melodic_req);
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

Melodic_req::Melodic_req()
{
    name = 0;
    octave = 0;
    accidental = 0;
    forceacc = false;
}

int
Melodic_req::height() const
{
    return  name + octave*7;
}

Rhythmic_req::Rhythmic_req()
{
    plet_factor = 1;
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
    
    mtor<<"xPlet factor"<<plet_factor<<"\n";
}

void
Melodic_req::print() const
{
    mtor << "note: " << name << " oct: "<< octave;
}

void
Note_req::print() const
{
    Melodic_req::print();
    Rhythmic_req::print();
}

void
Rest_req::print() const
{
    mtor << "rest, " ;
    Rhythmic_req::print();
}


Moment
Rhythmic_req::duration() const {    
    return wholes(balltype,dots)*plet_factor;
}

Beam_req::Beam_req()
{
    nplet = 0;
}

Span_req::Span_req()
{
    spantype = NOSPAN;
}

Script_req::Script_req(int d , Script_def*def)
{
    dir = d;
    scriptdef = def;
}

void
Script_req::print() const
{
    mtor << " dir " << dir ;
    scriptdef->print();
}


Script_req::~Script_req()
{
    delete scriptdef;
}


Text_req::Text_req(int d , Text_def*def)
{
    dir = d;
    spec = def;
}

void
Text_req::print() const
{
    mtor << " dir " << dir ;
    spec->print();
}


Text_req::~Text_req()
{
    delete spec;
}


