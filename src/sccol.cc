#include "sccol.hh"
#include "debug.hh"

Score_column::Score_column(Real w)
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
Real_compare(Real &a , Real& b)
{
    return sgn(a-b);
}

void
Score_column::preprocess()
{
    durations.sort(Real_compare);
}
void
Score_column::add_duration(Real d)
{
    for (int i = 0; i< durations.sz(); i++) {
	if (d == durations[i])
	    return ;
    }
    durations.add(d);
}
