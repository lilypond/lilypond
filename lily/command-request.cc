/*
  commandrequest.cc -- implement Nonmusical reqs

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "command-request.hh"
#include "debug.hh"
#include "musical-request.hh"


IMPLEMENT_STATIC_NAME(Cadenza_req);

void
Cadenza_req::do_print()const
{
    mtor << on_b_;
}

Cadenza_req::Cadenza_req(bool b)
{
    on_b_ =b;
}
/* *************** */


int
Bar_req::compare(Bar_req const &c1)const
{
    return type_str_ == c1.type_str_;
}

IMPLEMENT_STATIC_NAME(Bar_req);

void
Bar_req::do_print() const
{
    mtor << type_str_;
}

Bar_req::Bar_req(String s)
{
    type_str_ = s;
}

Partial_measure_req::Partial_measure_req(Moment m)
{
    duration_ =m;
}
/* *************** */

IMPLEMENT_STATIC_NAME(Timing_req);

void
Timing_req::do_print()const{}

IMPLEMENT_STATIC_NAME(Command_req);

void
Command_req::do_print()const{}
/* *************** */
IMPLEMENT_STATIC_NAME(Barcheck_req);

void
Barcheck_req::do_print() const{}

/* *************** */
IMPLEMENT_STATIC_NAME(Clef_change_req);

void
Clef_change_req::do_print() const
{
#ifndef NPRINT
    mtor << clef_str_ ;
#endif
}

Clef_change_req::Clef_change_req(String s)
{
    clef_str_ = s;
}
/* *************** */
IMPLEMENT_STATIC_NAME(Group_feature_req);

void
Group_feature_req::do_print() const
{
#ifndef NPRINT
    mtor << "stemdir " << stemdir_i_;
#endif
}

Group_feature_req::Group_feature_req()
{
    stemdir_i_ = 0;
}

IMPLEMENT_STATIC_NAME(Group_change_req);

void
Group_change_req::do_print()const
{
#ifndef NPRINT
    mtor << "id : " << newgroup_str_;
#endif
}
/* *************** */
IMPLEMENT_STATIC_NAME(Terminate_voice_req);

void
Terminate_voice_req::do_print()const
{
}

/* *************** */
IMPLEMENT_STATIC_NAME(Partial_measure_req);

void
Partial_measure_req::do_print() const
{
    mtor << duration_;
}

IMPLEMENT_STATIC_NAME(Meter_change_req);

void
Meter_change_req::do_print() const
{
    mtor << beats_i_ << "/" << one_beat_i_;
}

int
Meter_change_req::compare(Meter_change_req const &m)
{
    return m.beats_i_ == beats_i_ && one_beat_i_ == m.one_beat_i_;
}

Meter_change_req::Meter_change_req()
{
    beats_i_ = 0;
    one_beat_i_ =0;
}

void
Meter_change_req::set(int b,int o)
{
    beats_i_=b;
    one_beat_i_=o;
}

/* *************** */

IMPLEMENT_STATIC_NAME(Measure_grouping_req);

void
Measure_grouping_req::do_print() const
{
    for (int i=0; i < elt_length_arr_.size(); i++) {
	mtor << beat_i_arr_[i] <<"*" << elt_length_arr_[i]<<" ";
    }
}
/* *************** */

void
Key_change_req::transpose(Melodic_req const & d)const
{
    WARN << "don't know how to transpose a key. \n";
    for (int i=0; i < melodic_p_arr_.size(); i++) {
	melodic_p_arr_[i]->transpose(d);
    }
}

IMPLEMENT_STATIC_NAME(Key_change_req);
void
Key_change_req::squash_octaves()
{
    for (int i=0; i < melodic_p_arr_.size(); i++) {
	melodic_p_arr_[i]->octave_i_ = 0;
    }
}

void
Key_change_req::do_print() const
{
    for (int i=0; i < melodic_p_arr_.size(); i++) {
	melodic_p_arr_[i]->print();
    }
}

Key_change_req::Key_change_req()
{
	minor_b_ = false;
	multi_octave_b_= false;
}

Key_change_req::Key_change_req(Key_change_req const&c)
{
	for (int i=0; i < c.melodic_p_arr_.size(); i++) 
		melodic_p_arr_.push( c.melodic_p_arr_[i]->clone()->melodic() );
	minor_b_ = c.minor_b_;
	multi_octave_b_ = c.multi_octave_b_;
}

Key_change_req::~Key_change_req()
{
	for (int i=0; i < melodic_p_arr_.size(); i++)
		delete melodic_p_arr_[i];
}

int
Key_change_req::flats_i()
{
	int flats_i = 0;
	for ( int i = 0; i < melodic_p_arr_.size(); i++ ) {
		Melodic_req* mel_l = melodic_p_arr_[i]->melodic();
		assert( mel_l );
		if ( mel_l->accidental_i_ < 0 )
			flats_i -= mel_l->accidental_i_;
	}
	return flats_i;
}

int
Key_change_req::minor_b()
{
	return minor_b_;	
}

int
Key_change_req::sharps_i()
{
	int sharps_i = 0;
	for ( int i = 0; i < melodic_p_arr_.size(); i++ ) {
		Melodic_req* mel_l = melodic_p_arr_[i]->melodic();
		assert( mel_l );
		if ( mel_l->accidental_i_ > 0 )
			sharps_i+= mel_l->accidental_i_;
	}
	return sharps_i;
}

