#include "voice.hh"
#include "timedescription.hh"
#include "sccol.hh"
#include "staffcommands.hh"
#include "stcol.hh"

void
Staff_column::OK() const
{
#ifndef NDEBUG
    if (tdescription_) {
	assert(tdescription_->when == when());
	assert(*tdescription_ == staff_commands_p_->tdescription_);
    }
#endif
}

bool
Staff_column::mus() const
{
    return score_column_l_->musical_;
}

Moment
Staff_column::when() const
{
    return score_column_l_->when();
}

void
Staff_column::add(Voice_element*ve)
{
    Moment d= ve->duration;
    if (d){
	score_column_l_->add_duration(d);
    }
	
    v_elts.add(ve);
}

Staff_column::Staff_column(Score_column *s_l)
{
    tdescription_ =0;
    score_column_l_ = s_l;
    staff_commands_p_ = 0;
}

Staff_column::~Staff_column()
{
    delete tdescription_;
}
