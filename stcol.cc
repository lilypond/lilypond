#include "stcol.hh"
#include "sccol.hh"
#include "voice.hh"

bool
Staff_column::mus() const
{
    return score_column->musical;
}

Mtime
Staff_column::when() const
{
    return score_column->when;
}

void
Staff_column::add(Voice_element*ve)
{
    Mtime d= ve->duration;
    if (d){
	score_column->durations.add(d);
    }
	
    v_elts.add(ve);
}

Staff_column::Staff_column(Score_column*s) {
    score_column = s;
}
