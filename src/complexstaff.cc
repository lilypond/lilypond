#include "request.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "clef.hh"
#include "staff.hh"
#include "command.hh"
#include "complexstaff.hh"
#include "sccol.hh" 
#include "complexwalker.hh"



Complex_column::Complex_column(Score_column*s, Complex_staff *rs)
    : Staff_column(s)
{
    staff_l_ = rs;
}

Complex_staff::Complex_staff()
{
    theline_l_ = 0;
}

void
Complex_column::setup_requests()
{
    for (int i = 0 ; i < v_elts.size(); i ++)
	for (iter_top(v_elts[i]->reqs,j); j.ok(); j++) {	    

	    if (j->barcheck()) {
		if (tdescription_->whole_in_measure) {
		    error("Barcheck failed, " + tdescription_->str());
		}
		continue;
	    }
	    if (j->mark())
		continue;
	    if (j->command())
		continue;
	    todo_l_arr_.push(j);
	}
}

Staff_column*
Complex_staff::create_col(Score_column*s)
{
    return new Complex_column(s,this);
}

void
Complex_staff::walk()
{
    for (Complex_walker sc(this); sc.ok(); sc++) {
	sc.col()->setup_requests();// TODO
	sc.process();
    }
}
