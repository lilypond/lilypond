#include "melodicstaff.hh"
#include "paper.hh"
#include "molecule.hh"
#include "linestaff.hh"
#include "rhythmstaff.hh"
#include "sccol.hh" 

void
Melodic_staff::set_output(PScore*ps)
{
    theline = new Linestaff(5,ps);
    Simple_staff::set_output(ps);
}

void
Melodic_column::typeset_command(Command *com, int breakst)
{
    Item *i = create_command_item(com);
    typeset_item(i, breakst);
}

void
Melodic_column::typeset_req(Request *rq)
{
    Item *i = create_req_item(rq);
    if (rq->note()) {
	int h = rq->note()->height();
	Real dy = staff_->paper()->interline()/2;
	i->output->translate(Offset(0,(h-2)*dy));
    }
    typeset_item(i);
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
