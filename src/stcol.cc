#include "voice.hh"
#include "timedescription.hh"
#include "sccol.hh"
#include "stcol.hh"

bool
Staff_column::mus() const
{
    return score_column->musical;
}

Moment
Staff_column::when() const
{
    return score_column->when;
}

void
Staff_column::add(Voice_element*ve)
{
    Moment d= ve->duration;
    if (d){
	score_column->add_duration(d);
    }
	
    v_elts.add(ve);
}

Staff_column::Staff_column(Score_column*s)
{
    score_column = s;
    s_commands = 0;
    tdescription_ = 0;
}

Staff_column::~Staff_column()
{
    delete tdescription_;
}
