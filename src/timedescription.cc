#include "timedescription.hh"
#include "debug.hh"
String
Time_description::str()const
{
    String s( "Time_description { ");
    s+=String( " cadenza: ") + cadenza_b_ + " at ";
    s+=when;
    s+="\nmeter " + String(whole_per_measure/one_beat) +":" +(1/one_beat);
    s+= "\nposition "+String( bars) + ":"+ whole_in_measure +"\n}\n";
    return s;
}

void
Time_description::print() const
{    
    mtor << str();
}
void
Time_description::OK() const
{
#ifdef NDEBUG
    if (!cadenza_b_)
	assert(whole_in_measure < whole_per_measure);
    assert(0 <= whole_in_measure);
    assert(one_beat);
#endif
}

void
Time_description::set_cadenza(bool b)
{
    if (cadenza_b_ && !b) {
	if (whole_in_measure) {
	    bars ++;
	    whole_in_measure = 0;
	}
    }
    cadenza_b_ = b ;
}

Time_description::Time_description(Moment dt, Time_description const *prev)
{
    if (prev) {
	assert(dt >= 0);
	*this = *prev;
	when +=  dt;
	whole_in_measure += dt;
	
	while ( !cadenza_b_ && whole_in_measure >= whole_per_measure ) {
	    whole_in_measure -= whole_per_measure;
	    bars ++;
	}
    } else {    			// default 4/4
	whole_per_measure = 1;
	whole_in_measure =0;
	one_beat = 0.25;
	when = 0.0;
	bars = 0;
	cadenza_b_ = false;
    }
}

void
Time_description::set_meter(int l, int o)
{
    assert(o);
    one_beat = 1/Moment(o);
    whole_per_measure = Moment(l) * one_beat;
    if(whole_in_measure)
	error_t("Meterchange should be at start of measure", *this);
}

void
Time_description::setpartial(Moment p)
{
    if (when)
	error_t ("Partial measure only allowed at beginning.", *this);
    if (p<0||p > whole_per_measure)
	error_t ("Partial measure has incorrect size", *this);
    whole_in_measure = whole_per_measure - p;
}

Moment
Time_description::barleft()
{
    assert(!cadenza_b_);
    return whole_per_measure-whole_in_measure;
}

int
Time_description::compare(Time_description &t1, Time_description&t2)
{
    int i = sign(t1.when-t2.when);

    if (!i) {
	assert(t1.bars==t2.bars);
	assert(t1.one_beat == t2.one_beat);
	assert(t1.whole_in_measure == t2.whole_in_measure);
	assert(t1.whole_per_measure == t2.whole_per_measure);
    }

    return i;
}
