#include "timedescription.hh"
#include "debug.hh"

void
Time_description::print() const
{    
    mtor << "Time_description { at "<<when<<'\n'; 
    mtor << "meter " << whole_per_measure << ":" << 1/one_beat 
	 << "\nposition "<< bars << ":" << whole_in_measure <<"\n}\n";
}
void
Time_description::OK() const
{
    assert(whole_in_measure < whole_per_measure && 0 <= whole_in_measure);
    assert(one_beat);
}
Time_description::Time_description(Moment dt, Time_description const *prev)
{
    if (prev) {
	assert(dt >0);
	*this = *prev;
	when += + dt;
	whole_in_measure += dt;
	while ( whole_in_measure >= whole_per_measure ) {
	    whole_in_measure -= whole_per_measure;
	    bars ++;
	}
    } else {    			// default 4/4
	whole_per_measure = 1;
	whole_in_measure =0;
	one_beat = 0.25;
	when = 0.0;
	bars = 0;
    }	
}

void
Time_description::set_meter(int l, int o)
{
    assert(o);
    one_beat = 1/Moment(o);
    whole_per_measure = Moment(l) * one_beat;
}

void
Time_description::setpartial(Moment p)
{
    if (when)
	error_t ("Partial measure only allowed at beginning.", when);
    if (p<0||p > whole_per_measure)
	error_t ("Partial measure has incorrect size", when);
    whole_in_measure = whole_per_measure - p;
}
Moment
Time_description::barleft()
{
return    whole_per_measure-whole_in_measure;
}
