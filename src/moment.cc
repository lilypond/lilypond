#include "moment.hh"
#include "debug.hh"

void
Moment::print() const
{    
    mtor << " at "<<when<<'\n'; 
    mtor << "meeter " << whole_per_measure << "/" << 1/one_beat 
	 << "\nposition "<< bars << ":" << whole_in_measure <<'\n';
}
void
Moment::OK() const
{
    assert(whole_in_measure < whole_per_measure && 0 <= whole_in_measure);
    assert(one_beat);
}
Moment::Moment(Real dt, Moment const *prev)
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
Moment::set_meter(int l, int o)
{
    assert(o);
    one_beat = 1/Real(o);
    whole_per_measure = Real(l) * one_beat;
}

void
Moment::setpartial(Real p)
{
    if (when)
	error_t ("Partial measure only allowed at beginning.", when);
    if (p<0||p > whole_per_measure)
	error_t ("Partial measure has incorrect size", when);
    whole_in_measure = whole_per_measure - p;
}
Real
Moment::barleft()
{
return    whole_per_measure-whole_in_measure;
}
