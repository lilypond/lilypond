/*
  request.cc -- implement all musical requests.

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musical-request.hh"
#include "misc.hh"
#include "debug.hh"
#include "script-def.hh"
#include "text-def.hh"
#include "voice.hh"
#include "voice-element.hh"

IMPLEMENT_STATIC_NAME(Stem_req);
void
Stem_req::do_print() const
{
#ifndef NPRINT
    Rhythmic_req::do_print();
    mtor << "dir : " << dir_i_;
#endif
}

Stem_req::Stem_req()
{
    dir_i_ = 0;
}

/* ************** */
IMPLEMENT_STATIC_NAME(Musical_req);
void
Musical_req::do_print()const{}
void
Tie_req::do_print()const{}

IMPLEMENT_STATIC_NAME(Request);

void Request::do_print() const{}

/* *************** */

void
Request::print() const
{
#ifndef NPRINT
    mtor << name() << " {";
    do_print();
    mtor << "}\n";
#endif
}
     


IMPLEMENT_STATIC_NAME(Span_req);

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
}
Request::Request(Request const&src)
    :Input(src)
{
    elt_l_ = 0;
}
/* *************** */
Spacing_req::Spacing_req()
{
    next = 0;
    distance = 0;
    strength = 0;
}
IMPLEMENT_STATIC_NAME(Spacing_req);

void
Spacing_req::do_print()const
{
#ifndef NPRINT
    mtor << "next " << next << "dist " << distance << "strength\n";
#endif
}

IMPLEMENT_STATIC_NAME(Blank_req);

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
}

void
Melodic_req::transpose(Melodic_req const & delta)
{
    int old_pitch = pitch();
    int delta_pitch = delta.pitch();
    octave_i_ += delta.octave_i_;
    notename_i_ += delta.notename_i_;
    while  (notename_i_ >= 7 ) {
	notename_i_ -= 7;
	octave_i_ ++;
    }
    int new_pitch = pitch();
    int delta_acc = new_pitch - old_pitch - delta_pitch;
    
    accidental_i_ -= delta_acc;
    if (abs(accidental_i_) > 2) {
	delta.warning("transposition makes accidental larger than 2");
    }
}

IMPLEMENT_STATIC_NAME(Melodic_req);

int
Melodic_req::compare(Melodic_req const&m1, Melodic_req const&m2)
{
    if (m1.octave_i_ != m2.octave_i_)
	return m1.octave_i_ -m2.octave_i_;
    else if (m1.notename_i_ != m2.notename_i_)
	return m1.notename_i_ - m2.notename_i_;
    else  if (m1.accidental_i_ != m2.accidental_i_)
	return m1.accidental_i_ - m2.accidental_i_;
    return 0;
}

void
Melodic_req::do_print() const
{
#ifndef NPRINT
    mtor << "notename: " << notename_i_ << " acc: " <<accidental_i_<<" oct: "<< octave_i_;
#endif
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

IMPLEMENT_STATIC_NAME(Plet_req);

void
Plet_req::do_print() const
{
#ifndef NPRINT
    mtor << "plet: " << type_c_ << ": " << dur_i_ << "/" << type_i_;
#endif
}

/* *************** */
int
Rhythmic_req::compare(Rhythmic_req const &r1, Rhythmic_req const &r2)
{
    return sign(r1.duration() - r2.duration());
}

void
Rhythmic_req::set_duration(Duration d)
{
    duration_ = d;
}

Rhythmic_req::Rhythmic_req()
{
}

IMPLEMENT_STATIC_NAME(Rhythmic_req);

void
Rhythmic_req::do_print() const
{
#ifndef NPRINT
    mtor << "duration { " <<duration_.str() << "}";
#endif
}


Moment
Rhythmic_req::duration() const {    
    return duration_.length();
}
/* *************** */

Lyric_req::Lyric_req(Text_def* def_p)
    :Text_req(0, def_p)
{
    def_p->align_i_ = 0;	// centre
    dir_i_ = -1;		// lyrics below (invisible) staff
}

IMPLEMENT_STATIC_NAME(Lyric_req);

void
Lyric_req::do_print() const
{    
    Rhythmic_req::do_print();
    Text_req::do_print();
}

/* *************** */
Note_req::Note_req()
{
    forceacc_b_ = false;
}
IMPLEMENT_STATIC_NAME(Note_req);

void
Note_req::do_print() const
{
#ifndef NPRINT
    Melodic_req::do_print();
    if (forceacc_b_) {
	mtor << " force accidental\n";
    }
    Rhythmic_req::do_print();
#endif
}
/* *************** */
IMPLEMENT_STATIC_NAME(Rest_req);

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
IMPLEMENT_STATIC_NAME(Beam_req);
void
Beam_req::do_print()const{}
/* *************** */
IMPLEMENT_STATIC_NAME(Slur_req);
void
Slur_req::do_print()const{}
/* *************** */
int
Span_req:: compare(Span_req const &r1, Span_req const &r2)
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
Script_req::compare(Script_req const &d1, Script_req const &d2)
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

IMPLEMENT_STATIC_NAME(Script_req);

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
Text_req:: compare(Text_req const &r1, Text_req const &r2)
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

IMPLEMENT_STATIC_NAME(Text_req);

void
Text_req::do_print() const
{
#ifndef NPRINT

    mtor << " dir " << dir_i_ ;
    tdef_p_->print();
#endif
}

/* *************** */

Moment
Skip_req::duration() const
{
    return duration_;
}

IMPLEMENT_STATIC_NAME(Skip_req);

void
Skip_req::do_print() const
{
#ifndef NPRINT

    mtor << "duration: " << duration();
#endif
}

Voice *
Request::voice_l()
{
    if (!elt_l_)
	return 0;
    else
	return (Voice*)elt_l_->voice_C_;
}
/* *************** */

IMPLEMENT_STATIC_NAME(Subtle_req);

void
Subtle_req::do_print() const
{
#ifndef NPRINT
	mtor << " subtime " <<  subtime_;
#endif
}

IMPLEMENT_STATIC_NAME(Dynamic_req);

void
Dynamic_req::do_print() const
{
    Subtle_req::do_print();
}

IMPLEMENT_STATIC_NAME(Absolute_dynamic_req);

void
Absolute_dynamic_req::do_print() const
{
    Dynamic_req::do_print();
    mtor << " loudness " <<loudness_;
}

String
Dynamic_req::loudness_str(Loudness l) 
{
    switch (l) {
    case FFF: return "fff";
    case FF: return "ff";
    case F: return "f";
    case MF: return "mf";
    case MP: return "mp";
    case P: return "p";
    case PP: return "pp";
    case PPP: return "ppp";
    }
    assert(false);
    return "";
}

Absolute_dynamic_req::Absolute_dynamic_req()
{
    loudness_ = MF;
}


Span_dynamic_req::Span_dynamic_req()
{
    dynamic_dir_i_  = 0;
}

IMPLEMENT_STATIC_NAME(Span_dynamic_req);

void
Span_dynamic_req::do_print()const
{
#ifndef NPRINT
    Span_req::do_print();
    mtor << "louder/louder: " <<dynamic_dir_i_;
#endif
}

IMPLEMENT_STATIC_NAME(Tie_req);
