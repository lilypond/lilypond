#include "lookup.hh"
#include "request.hh"
#include "beam.hh"
#include "pscore.hh"
#include "paper.hh"
#include "simplestaff.hh"
#include "molecule.hh"
#include "sccol.hh"
#include "stem.hh"
#include "notehead.hh"
#include "rest.hh"
#include "debug.hh"
#include "bar.hh"
#include "meter.hh"

Item *
Simple_staff::get_TYPESET_item(Command *com)
{
    Item *s;
    if (com -> args[0] ==  "BAR" ) {
	s = new Bar(com->args[1]);	
    } else if (com->args[0] == "METER") {
	svec<String> arg( com->args);	
	arg.del(0);
	s = new Meter(arg);
    } else
	assert(false);
    
    return s;
}

void
Simple_column::typeset_item(Item *i, int breakst)
{
    assert(i);
    // ugh
    staff_->pscore_->typeset_item(i, score_column->pcol,
				  staff_->theline,breakst);
}

void
Simple_staff::set_output(PScore* ps )
{
    pscore_ = ps;
    pscore_->add(theline);
}


Rest*
Simple_staff::get_rest(Rest_req*rq)
{
    int b = rq->rhythmic()->balltype;
    int d = rq->rhythmic()->dots;
    return new Rest(b, d);  
}
