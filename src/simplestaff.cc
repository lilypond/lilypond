#include "request.hh"
#include "swalker.hh"
#include "debug.hh"
#include "staff.hh"
#include "command.hh"
#include "simplestaff.hh"
#include "sccol.hh" 




Simple_column::Simple_column(Score_column*s, Simple_staff *rs)
    : Staff_column(s)
{
    stem_ = 0;    
    staff_ = rs;
    beam_ = 0;
}

Simple_staff::Simple_staff()
{
    theline = 0;
}

/**
 accept:

    BREAK: all
    TYPESET: bar, meter,

    */



void
Simple_column::process_requests()
{
    for (int i = 0 ; i < v_elts.sz(); i ++)
	for (PCursor<Request *> rqc(v_elts[i]->reqs); rqc.ok(); rqc++) {
	    Request *rq= rqc;
	    if (rq->rhythmic()){
		notes.add( rq->rhythmic());
	    }
	    if (rq->stem()) {
		stem_ = rq->stem();
	    }

	    if (rq->beam()) {
		beam_ = rq->beam();
	    }
	}
}
Staff_column*
Simple_staff::create_col(Score_column*s)
{
    return new Simple_column(s,this);
}
void
Simple_staff::walk()
{
    for (Simple_walker sc(this); sc.ok(); sc++) {
	sc.col()->process_requests();// TODO
	sc.process();
    }
}

