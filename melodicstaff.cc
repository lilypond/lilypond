#include "melodicstaff.hh"
#include "stem.hh"

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
    Item *i =new Item;
    Molecule*m=create_req_mol(rq);

    if (rq->note()) {
	int h = rq->note()->height();
	Real dy = staff_->paper()->interline()/2;
	m->translate(Offset(0,(h-BOTTOM_POSITION)*dy));
    }
    i->output = m;
    typeset_item(i);
}


void
Melodic_column::typeset_stem(Stem_req*rq)
{
    Stem * s = new Stem(NO_LINES);
    int n = the_note->note()->height()-BOTTOM_POSITION;
    s->minnote =s->maxnote=n;
    s->flag = rq->stem_number;
    s->calculate();
    typeset_item(s);
    
    s->brew_molecole();
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
