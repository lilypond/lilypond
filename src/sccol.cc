#include "debug.hh"
#include "pcol.hh"
#include "sccol.hh"

int
Score_column::compare(Score_column & c1, Score_column &c2)
{
	return sign(c1.when - c2.when);
}

void
Score_column::set_breakable() {
    pcol_->set_breakable();
}
Score_column::Score_column(Moment w)
{
    when = w;
    pcol_ = new PCol(0);
    musical = false;
}

bool
Score_column::used() {
    return pcol_->used();
}

void
Score_column::print() const
{
#ifndef NPRINT
    mtor << "Score_column { mus "<< musical <<" at " <<  when<<'\n';
    mtor << "durations: [";
    for (int i=0; i < durations.sz(); i++)
	mtor << durations[i] << " ";
    mtor << "]\n";
    pcol_->print();
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
    for (int i = 0; i< durations.sz(); i++) {
	if (d == durations[i])
	    return ;
    }
    durations.add(d);
}
