/*
  commandrequest.cc -- implement Nonmusical reqs

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "command-request.hh"
#include "debug.hh"
#include "musical-request.hh"


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

void
Timing_req::do_print()const{}

void
Command_req::do_print()const{}
/* *************** */
void
Barcheck_req::do_print() const{}

/* *************** */
void
Clef_change_req::do_print() const
{
    mtor << clef_str_ ;
}

Clef_change_req::Clef_change_req(String s)
{
    clef_str_ = s;
}
/* *************** */
void
Group_feature_req::do_print() const
{
    mtor << "stemdir " << stemdir_i_;
}

Group_feature_req::Group_feature_req()
{
    stemdir_i_ = 0;
}

void
Group_change_req::do_print()const
{
    mtor << "id : " << newgroup_str_;
}
/* *************** */
void
Terminate_voice_req::do_print()const
{
}

/* *************** */
void
Partial_measure_req::do_print() const
{
    mtor << duration_;
}

void
Meter_change_req::do_print() const
{
    mtor << beats_i_ << "*" << one_beat_i_;
}

/* *************** */

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

void
Key_change_req::do_print() const
{
    for (int i=0; i < melodic_p_arr_.size(); i++) {
	melodic_p_arr_[i]->print();
    }
}

Key_change_req::Key_change_req()
{
}
Key_change_req::Key_change_req(Key_change_req const&c)
{
    for (int i=0; i < c.melodic_p_arr_.size(); i++) {
	melodic_p_arr_.push( c.melodic_p_arr_[i]->clone()->melodic() );
    }
}

Key_change_req::~Key_change_req()
{
    for (int i=0; i < melodic_p_arr_.size(); i++)
	delete melodic_p_arr_[i];
}
