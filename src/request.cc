#include "request.hh"
#include "misc.hh"
#include "debug.hh"
#include "scriptdef.hh"
#include "textdef.hh"

#include "inputcommand.hh"

#define VIRTUALCONS(T,R) R *T::clone() const { return  new T(*this); } struct T
#define RCONS(T) VIRTUALCONS(T, Request)

RCONS(Rest_req);
RCONS(Barcheck_req);
RCONS(Text_req);
RCONS(Rhythmic_req);
RCONS(Lyric_req);
RCONS(Mark_req);
RCONS(Stem_req);
RCONS(Script_req);
RCONS(Note_req);
RCONS(Melodic_req);
RCONS(Span_req);
RCONS(Slur_req);
RCONS(Beam_req);
RCONS(Staff_command_req);

void
Stem_req::print() const
{
    mtor << "Stem\n";
}
/****************/
void
Barcheck_req::print() const    
{
#ifndef NPRINT
    mtor << "Barcheck\n";
#endif
}
/****************/
void
Request::print() const    
{
#ifndef NPRINT
    mtor << "Req{ unknown }\n";
#endif
}

void
Span_req::print() const    
{
#ifndef NPRINT
    mtor << "Span_req {" << spantype << "}\n";
#endif
}

Request::Request()
{
    elt_l_ = 0;
}
Request::Request(Request const&)
{
    elt_l_ = 0;
}
/****************/
Melodic_req::Melodic_req()
{
    name = 0;
    octave = 0;
    accidental = 0;
    forceacc = false;
}

void
Melodic_req::print() const
{
    mtor << "note: " << name << " oct: "<< octave;
}

int
Melodic_req::height() const
{
    return  name + octave*7;
}

/****************/
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
    
    mtor<<", plet factor"<<plet_factor<<"\n";
}


Moment
Rhythmic_req::duration() const {    
    return wholes(balltype,dots)*plet_factor;
}
/****************/

Lyric_req::Lyric_req(Text_def* def_p)
    :Text_req(0, def_p)
{
    def_p->align_i_ = 1;	// raggedright
    dir_i_ = -1;		// lyrics below (invisible) staff
}

void
Lyric_req::print() const
{
    mtor << "lyric: ";
    Rhythmic_req::print();
    Text_req::print();
}
/****************/
void
Note_req::print() const
{
    Melodic_req::print();
    Rhythmic_req::print();
}
/****************/
void
Rest_req::print() const
{
    mtor << "rest, " ;
    Rhythmic_req::print();
}
/****************/
Beam_req::Beam_req()
{
    nplet = 0;
}
/****************/
Span_req::Span_req()
{
    spantype = NOSPAN;
}
/****************/
Script_req::Script_req(int d , Script_def*def)
{
    dir = d;
    scriptdef = def;
}

Script_req::Script_req(Script_req const &s)
{
    dir = s.dir;
    scriptdef = new Script_def(*s.scriptdef);
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
/****************/

Text_req::~Text_req()
{
    delete tdef_p_;
    tdef_p_ = 0;
}

Text_req::Text_req(Text_req const& src)
{
    tdef_p_ = new Text_def(*src.tdef_p_);
    dir_i_ = src.dir_i_;
}

Text_req::Text_req(int dir_i, Text_def* tdef_p)	
{
    dir_i_ = dir_i;
    tdef_p_ = tdef_p;
}

void
Text_req::print() const
{
    mtor << " dir " << dir_i_ ;
    tdef_p_->print();
}



/****************/

Mark_req::Mark_req(String s)
{
    mark_str_ = s;
}

void
Mark_req::print()const
{
#ifndef NDEBUG
    mtor<< "Mark `" << mark_str_ << "\'\n";
#endif
}
/****************/
Staff_command_req::Staff_command_req(Input_command * p)
{
    com_p_ = p;
}
Staff_command_req::~Staff_command_req()
{
    delete com_p_;
}
Staff_command_req::Staff_command_req(Staff_command_req const&src)
{
    com_p_ = new Input_command(*src.com_p_);
}
void
Staff_command_req::print()const
{
    mtor << "Command request: " ;
    com_p_->print();
}


