/*
  time-description.cc -- implement Time_description

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "time-description.hh"
#include "debug.hh"

String
Time_description::str()const
{
    String s( "Time_description { ");
    if (cadenza_b_)
	s+=String( " (cadenza) ");
    s+= "at ";
    s+=when_;
    s+="\nmeter " + String(whole_per_measure_/one_beat_) +":" +
	String(Rational(Rational(1)/one_beat_));
    s+= "\nposition "+String( bars_i_) + ":"+ whole_in_measure_ +"\n}\n";
    return s;
}

void
Time_description::print() const
{
#ifndef NPRINT
    mtor << str();
#endif
}
void
Time_description::OK() const
{
#ifndef NDEBUG
    if (!cadenza_b_)
	assert(whole_in_measure_ < whole_per_measure_);
    assert(0 <= whole_in_measure_);
    assert(one_beat_);
#endif
}

void
Time_description::set_cadenza(bool b)
{
    if (cadenza_b_ && !b) {
	if (whole_in_measure_) {
	    bars_i_ ++;
	    whole_in_measure_ = 0;
	}
    }
    cadenza_b_ = b ;
}

Time_description::Time_description()
{
    whole_per_measure_ = 1;
    whole_in_measure_ =0;
    one_beat_ = Moment(1,4);
    when_ = 0;
    bars_i_ = 0;
    cadenza_b_ = false;
}

void
Time_description::add(Moment dt)
{
    assert(dt >= Rational(0));
    when_ +=  dt;
    whole_in_measure_ += dt;
	
    while ( !cadenza_b_ && whole_in_measure_ >= whole_per_measure_ ) {
	whole_in_measure_ -= whole_per_measure_;
	bars_i_ ++;
    }
}

void
Time_description::set_meter(int l, int o)
{
    assert(o);
    one_beat_ = Rational(1)/Moment(o);
    whole_per_measure_ = Moment(l) * one_beat_;
    if(whole_in_measure_)
	error_t("Meterchange should be at start of measure", *this);
}

void
Time_description::setpartial(Moment p)
{
    if (when_)
	error_t ("Partial measure only allowed at beginning.", *this);
    if (p<Rational(0)||p > whole_per_measure_)
	error_t ("Partial measure has incorrect size", *this);
    whole_in_measure_ = whole_per_measure_ - p;
}

Moment
Time_description::barleft()
{
    assert(!cadenza_b_);
    return whole_per_measure_-whole_in_measure_;
}

int
Time_description::compare(Time_description &t1, Time_description&t2)
{
    int i = sign(t1.when_-t2.when_);

    if (!i) {
	assert(t1.bars_i_==t2.bars_i_);
	assert(t1.one_beat_ == t2.one_beat_);
	assert(t1.whole_in_measure_ == t2.whole_in_measure_);
	assert(t1.whole_per_measure_ == t2.whole_per_measure_);
    }

    return i;
}
