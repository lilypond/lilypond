#include "request.hh"
#include "debug.hh"
#include "linestaff.hh"
#include "staff.hh"
#include "pstaff.hh"
#include "pscore.hh"
#include "command.hh"
#include "molecule.hh"
#include "rhythmstaf.hh"
#include "lookupsyms.hh"
#include "sccol.hh" 

Rhythmic_column::Rhythmic_column(Score_column*s, Rhythmic_staff *rs)
    : Staff_column(s)
{
    the_note = 0;
    staff_ = rs;
}

Rhythmic_staff::Rhythmic_staff()
{
    theline = new Linestaff(1);
}
void
Rhythmic_staff::set_output(PScore* ps )
{
    pscore_ = ps;
    pscore_->add(theline);
}

// should integrate handling of BREAK commands into Staff_column
void
Rhythmic_column::process_commands( )
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
Rhythmic_column::process_requests()
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
Rhythmic_column::typeset_command(Command *com, int breakst)
{
    Item *i = new Item;
    Symbol s;

    if (com -> args[0] ==  "BAR" ) {
	s = Lookup::bar(com->args[1]);	
    } else if (com->args[0] == "METER") {
	Parametric_symbol *p = Lookup::meter("general");
	svec<String> arg( com->args);
	arg.del(0);
	s = p->eval(arg);
    } else
	assert(false);

    Molecule * m =new Molecule(Atom(s));
    {
	Interval wid;
	svec<Item*> sv(staff_->pscore_->
		       select_items(staff_->theline, score_column->pcol));
	for (int j=0; j<sv.sz(); j++) {
	    wid.unite(sv[j]->output->extent().x);
	}
	if (!wid.empty())
	    m->translate(Offset(wid.max,0));
    }
    i->output=m;
    staff_->pscore_->typeset_item(i, score_column->pcol,
				  staff_->theline,breakst);
}

void
Rhythmic_column::typeset_req(Request *rq)
{
    Item *i = new Item;
    Symbol s;
    int dots=0;

    if (rq->note())
	s = Lookup::ball(rq->note()->balltype);
    if (rq->rhythmic())
	dots=rq->rhythmic()->dots;
    if (rq->rest())
	s = Lookup::rest(rq->rest()->balltype);

    Molecule *m = new Molecule(Atom(s));
    if (dots) {
	Symbol d = Lookup::dots(dots);
	Molecule dm;
	dm.add(Atom(d));
	m->add_right(dm);
    }
    i->output=m;
    staff_->pscore_->typeset_item(i, score_column->pcol, staff_->theline );
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
