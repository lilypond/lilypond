#include "sccol.hh"
#include "debug.hh"

Score_column::Score_column(Real w)
{
    when = w;
    pcol = new PCol(0);
    musical = false;
}

bool
Score_column::used() {
    return pcol->used;
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
    pcol->print();
    mtor << "}\n";
#endif
}
