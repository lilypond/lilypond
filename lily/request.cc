/*
  request.cc -- implement all musical requests.

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musicalrequest.hh"
#include "misc.hh"
#include "debug.hh"
#include "script-def.hh"
#include "text-def.hh"
#include "voice.hh"
#include "voice-element.hh"

void
Stem_req::do_print() const
{
    Rhythmic_req::do_print();
    mtor << "dir : " << dir_i_;
}

Stem_req::Stem_req(int s, int d)
    : Rhythmic_req(s,d)
{
    dir_i_ = 0;
}

/* ************** */
void Musical_req::do_print()const{}
void Request::do_print() const{}

/* *************** */

void
Request::print() const
{
    mtor << name() << " {";
    do_print();
    mtor << "}\n";
}
     


void
Span_req::do_print() const    
{
#ifndef NPRINT
    mtor  << spantype ;
#endif
}

Request::Request()
{
    elt_l_ = 0;
    defined_ch_c_l_ = 0;
}
Request::Request(Request const&src)
{
    elt_l_ = 0;
    defined_ch_c_l_ = src.defined_ch_c_l_;
}
/* *************** */
Spacing_req::Spacing_req()
{
    next = 0;
    distance = 0;
    strength = 0;
}
void
Spacing_req::do_print()const
{
    mtor << "next " << next << "dist " << distance << "strength\n";
}

void
Blank_req::do_print()const
{
    Spacing_req::do_print();
}
/* *************** */
Melodic_req::Melodic_req()
{
    notename_i_ = 0;
    octave_i_ = 0;
    accidental_i_ = 0;
    forceacc_b_ = false;
}

void
Melodic_req::do_print() const
{
    mtor << "notename: " << notename_i_ << " acc: " <<accidental_i_<<" oct: "<< octave_i_;
}

int
Melodic_req::height() const
{
    return  notename_i_ + octave_i_*7;
}

/*
 should be settable from input to allow "viola"-mode
 */
static Byte pitch_byte_a[ 7 ] = { 0, 2, 4, 5, 7, 9, 11 };	

int
Melodic_req::pitch() const
{
    return  pitch_byte_a[ notename_i_ % 7 ] + accidental_i_ + octave_i_ * 12;
}

Plet_req::Plet_req()
{
    type_c_ = ']';
    dur_i_ = 1;
    type_i_ = 1;
}

void
Plet_req::do_print() const
{
    mtor << "plet: " << type_c_ << ": " << dur_i_ << "/" << type_i_;
}

/* *************** */
int
Rhythmic_req::compare(const Rhythmic_req &r1, const Rhythmic_req &r2)
{
    return sign(r1.duration() - r2.duration());
}
Rhythmic_req::Rhythmic_req(int b, int d)
{
    plet_factor = 1;
    balltype = b;
    dots = d;
}

Rhythmic_req::Rhythmic_req()
{
    plet_factor = 1;
    balltype = 1;
    dots = 0;
}

void
Rhythmic_req::do_print() const
{
    mtor << "ball: " << balltype ;
    int d =dots;
    while (d--)
	mtor << '.';
    
    mtor<<", plet factor"<<plet_factor<<"\n";
}


Moment
Rhythmic_req::duration() const {    
    return wholes(balltype,dots)*plet_factor;
}
/* *************** */

Lyric_req::Lyric_req(Text_def* def_p)
    :Text_req(0, def_p)
{
    def_p->align_i_ = 0;	// centre
    dir_i_ = -1;		// lyrics below (invisible) staff
}

void
Lyric_req::do_print() const
{    
    Rhythmic_req::do_print();
    Text_req::do_print();
}
/* *************** */
void
Note_req::do_print() const
{
    Melodic_req::do_print();
    Rhythmic_req::do_print();
}
/* *************** */
void
Rest_req::do_print() const
{
        Rhythmic_req::do_print();
}

/* *************** */
Beam_req::Beam_req()
{
    nplet = 0;
}

void Beam_req::do_print()const{}
/* *************** */
void Slur_req::do_print()const{}
/* *************** */
int
Span_req:: compare(const Span_req &r1, const Span_req &r2)
{
     return r1.spantype - r2.spantype;
}

Span_req::Span_req()
{
    spantype = NOSPAN;
}

/* *************** */
Script_req::Script_req(int d , Script_def*def)
{
    dir_i_ = d;
    scriptdef_p_ = def;
}

int
Script_req::compare(const Script_req &d1, const Script_req &d2)
{
    return d1.dir_i_ == d2.dir_i_ &&
	d1.scriptdef_p_->compare(*d2.scriptdef_p_);
}

Script_req::Script_req(Script_req const &s)
    : Request( s )
{
    dir_i_ = s.dir_i_;
    scriptdef_p_ = new Script_def(*s.scriptdef_p_);
}

void
Script_req::do_print() const
{
    mtor << " dir " << dir_i_ ;
    scriptdef_p_->print();
}


Script_req::~Script_req()
{
    delete scriptdef_p_;
}
/* *************** */
int
Text_req:: compare(const Text_req &r1, const Text_req &r2)
{
    bool b1 = (r1.dir_i_ == r2.dir_i_);
    bool b2 = (r1.tdef_p_ ->compare(*r2.tdef_p_));
    return b1 && b2;
}
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
Text_req::do_print() const
{
    mtor << " dir " << dir_i_ ;
    tdef_p_->print();
}

/* *************** */

Moment
Skip_req::duration() const
{
    return duration_;
}

void
Skip_req::do_print() const
{
    mtor << "duration: " << duration();
}

Voice *
Request::voice_l()
{
    if (!elt_l_)
	return 0;
    else
	return (Voice*)elt_l_->voice_l_;
}
