#include "request.hh"
#include "debug.hh"
#include "staff.hh"
#include "command.hh"
#include "simplestaff.hh"
#include "sccol.hh" 




Simple_column::Simple_column(Score_column*s, Simple_staff *rs)
    : Staff_column(s)
{
    the_note = 0;
    staff_ = rs;
}

Simple_staff::Simple_staff()
{
    theline = 0;
}

// should integrate handling of BREAK commands into Staff_column
void
Simple_column::process_commands( )
{
    int breakstat = BREAK_END - BREAK_PRE;
    for (int i = 0 ; i < s_commands.sz(); i++) {
	Command *com = s_commands[i];
	switch (com->code){
	case INTERPRET:
	    break;
	case BREAK_PRE:
	case BREAK_MIDDLE:
	case BREAK_POST:
	case BREAK_END:
	    score_column->set_breakable();
	    breakstat = com->code- BREAK_PRE;
	    break;
	    
	case TYPESET:
	    typeset_command ( com , breakstat);
	    break;
	default :
	    break;
	}	
    }
}
/**
 accept:

    BREAK: all
    TYPESET: bar, meter

    */



void
Simple_column::process_requests()
{
    for (int i = 0 ; i < v_elts.sz(); i ++)
	for (PCursor<Request *> rqc(v_elts[i]->reqs); rqc.ok(); rqc++) {
	    Request *rq= rqc;
	    if (rq->rhythmic()){
		if (the_note){
		    WARN << "too many notes.\n";
		    return;
		}
		the_note = rq;
	    }
	    break;
	}
}

void
Simple_staff::grant_requests()
{
    for  (PCursor<Staff_column*> cc(cols); cc.ok(); cc++) {
	Simple_column *rp = (Simple_column*)*cc;
	if (rp->the_note)
	    rp->typeset_req( rp->the_note);
    }
}


