#include "stcol.hh"
#include "sccol.hh"
#include "voice.hh"
#include "moment.hh"

bool
Staff_column::mus() const
{
    return score_column->musical;
}

Real
Staff_column::when() const
{
    return score_column->when;
}

void
Staff_column::add(Voice_element*ve)
{
    Real d= ve->duration;
    if (d){
	score_column->add_duration(d);
    }
	
    v_elts.add(ve);
}

Staff_column::Staff_column(Score_column*s)
{
    score_column = s;
    s_commands = 0;
    moment_ = 0;
}

Staff_column::~Staff_column()
{
    delete moment_;
}
