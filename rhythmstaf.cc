#include "request.hh"
#include "debug.hh"
#include "linestaff.hh"
#include "staff.hh"
#include "pstaff.hh"
#include "pscore.hh"
#include "command.hh"
#include "molecule.hh"
#include "rhythmstaf.hh"

 

Rhythmic_column::Rhythmic_column(Score_column*s, Rhythmic_staff *rs)
    : Staff_column(s)
{
    the_note = 0;
    staff_ = rs;
}


void
Rhythmic_staff::set_output(PScore* ps )
{
    theline = new Linestaff(1);
    pscore_ = ps;
    pscore_->add(theline);
}

// should integrate handling of BREAK commands into Staff_column
void
Rhythmic_column::process_commands( )
{
    int breakstat = BREAK_END;
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
	    breakstat = com->code;
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
Rhythmic_column::process_requests()
{
    for (int i = 0 ; i < v_elts.sz(); i ++)
	for (PCursor<Request *> rqc(v_elts[i]->reqs); rqc.ok(); rqc++) {
	    Request *rq= rqc;
	    switch(rq->tag){
	    case Request::NOTE:
	    case Request::REST:
		if (the_note){
		    WARN << "too many notes.\n";
		    return;
		}
		the_note = rq;
		break;
		
	    default:
		return;
	    }
	}
    
}


void
Rhythmic_column::typeset_command(Command *com, int breakst)
{
    Item *i = new Item;
    const Symbol*s=0;

    if (com -> args[0] ==  "|" ) {
	s = Symbol::find_bar("|");	
    } else
	assert(false);
    
    i->output=new Molecule(Atom(s));
    staff_->pscore_->typeset_item(i, score_column->pcol,
				  staff_->theline,breakst);
}

void
Rhythmic_column::typeset_req(Request *rq)
{
    Item *i = new Item;
    const Symbol*s=0;

    switch(rq->tag){
    case Request::NOTE:
	s = Symbol::find_ball(rq->note()->balltype);
	break;
    case Request::REST:
	s = Symbol::find_rest(rq->rest()->balltype);
	break;
    default:
	assert(false);
	break;
    }  
    i->output = new Molecule(Atom(s));

    staff_->pscore_->typeset_item(i, score_column->pcol, staff_->theline,0 );	
}

void
Rhythmic_staff::grant_requests()
{
    for  (PCursor<Staff_column*> cc(cols); cc.ok(); cc++) {
	Rhythmic_column *rp = (Rhythmic_column*)*cc;
	if (rp->the_note)
	    rp->typeset_req( rp->the_note);
    }
}

Staff_column*
Rhythmic_staff::create_col(Score_column*s)
{
    Rhythmic_column *rc = new Rhythmic_column(s,this);

    return rc;
}

Staff *
get_new_rhythmstaff()
{
    return new Rhythmic_staff;
}
