#include "melodicstaff.hh"
#include "stem.hh"
#include "notehead.hh"
#include "paper.hh"
#include "molecule.hh"
#include "linestaff.hh"
#include "rhythmstaff.hh"
#include "sccol.hh" 

const int NO_LINES=5;
const int BOTTOM_POSITION=2;	// e is on bottom line of 5-staff...

void
Melodic_staff::set_output(PScore*ps)
{
    theline = new Linestaff(NO_LINES,ps);
    Simple_staff::set_output(ps);
}

void
Melodic_column::typeset_command(Command *com, int breakst)
{
    Molecule*m=create_command_mol(com);
    Item *i =new Item;
    i->output = m;
    typeset_item(i, breakst);
}

void
Melodic_column::typeset_req(Request *rq)
{
    Item *i ;
    if (rq->note()) {
	Notehead *n =new Notehead((NO_LINES-1)*2);
	n->balltype = rq->rhythmic()->balltype;
	n->dots = rq->rhythmic()->dots;
	n->position = rq->note()->height() - BOTTOM_POSITION;
	i = n;
    } else if (rq->rest()) {
	i =new Item;
	Molecule*m=create_req_mol(rq);
	i->output=m;
    }
    typeset_item(i);
}


void
Melodic_column::typeset_stem(Stem_req*rq)
{
    Stem * s = new Stem(NO_LINES);
    int n = the_note->note()->height()-BOTTOM_POSITION;
    s->minnote =s->maxnote=n;
    s->flag = rq->stem_number;
    typeset_item(s);   
}

/*
  creation
  */
Staff *
get_new_melodicstaff()
{
    return new Melodic_staff;
}


Staff_column*
Melodic_staff::create_col(Score_column*s)
{
    return new Melodic_column(s,this);
}

Melodic_staff*
Melodic_staff::clone()const
{
    return new Melodic_staff(*this);
}
