#include "debug.hh"
#include "pcol.hh"
#include "sccol.hh"

int
Score_column::compare(Score_column & c1, Score_column &c2)
{
	return sign(c1.when_ - c2.when_);
}

void
Score_column::set_breakable()
{
    pcol_l_->set_breakable();
}

Score_column::Score_column(Moment w)
{
    when_ = w;
    pcol_l_ = new PCol(0);
    musical_ = false;
}

bool
Score_column::used() {
    return pcol_l_->used();
}

void
Score_column::print() const
{
#ifndef NPRINT
    mtor << "Score_column { mus "<< musical_ <<" at " <<  when_<<'\n';
    mtor << "durations: [";
    for (int i=0; i < durations.size(); i++)
	mtor << durations[i] << " ";
    mtor << "]\n";
    pcol_l_->print();
    mtor << "}\n";
#endif
}

int
Tdescription_compare(Moment &a , Moment& b)
{
    return sign(a-b);
}

void
Score_column::preprocess()
{
    durations.sort(Tdescription_compare);
}
void
Score_column::add_duration(Moment d)
{
    assert(d);
    for (int i = 0; i< durations.size(); i++) {
	if (d == durations[i])
	    return ;
    }
    durations.push(d);
}
