#include "molecule.hh"
#include "notehead.hh"
#include "stem.hh"
#include "linestaff.hh"
#include "rhythmstaff.hh"
#include "paper.hh"
#include "sccol.hh" 


void
Rhythmic_staff::set_output(PScore*ps)
{
    theline = new Linestaff(1,ps);
    Simple_staff::set_output(ps);
}


void
Rhythmic_column::typeset_command(Command *com, int breakst)
{
    Item *i =new Item;
    Molecule*m = create_command_mol(com);
    i->output=m;
    m->translate(Offset(0,
			-staff_->score_->paper_->standard_height()/2));
    typeset_item(i, breakst);
}

void
Rhythmic_column::typeset_req(Request *rq)
{
    Item *i ;
    if (rq->note()) {
	Notehead *n =new Notehead(1);
	n->balltype = rq->rhythmic()->balltype;
	n->dots = rq->rhythmic()->dots;
	n->position = 0;
	i = n;
    } else if (rq->rest()) {
	i =new Item;
	Molecule*m=create_req_mol(rq);
	i->output=m;
    }
    typeset_item(i);
}

void
Rhythmic_column::typeset_stem(Stem_req*rq)
{
    Stem * s = new Stem(0);
    s->minnote = s->maxnote = 0;
    s->flag = rq->stem_number;
    typeset_item(s);
}

/*
  creation
  */
Staff *
get_new_rhythmstaff()
{
    return new Rhythmic_staff;
}


Staff_column*
Rhythmic_staff::create_col(Score_column*s)
{
    return new Rhythmic_column(s,this);
}

Rhythmic_staff*
Rhythmic_staff::clone() const
{
    return new Rhythmic_staff(*this);
}
