#include "molecule.hh"
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
    Item *i = create_command_item(com);
    i->output->translate(Offset(0,
				-staff_->score_->paper_->standard_height()/2));
    typeset_item(i, breakst);
}

void
Rhythmic_column::typeset_req(Request *rq)
{
    Item *i =create_req_item(rq);
    typeset_item(i);
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
