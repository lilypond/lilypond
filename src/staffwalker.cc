#include "request.hh"
#include "staffwalker.hh"
#include "stcol.hh"
#include "sccol.hh"
#include "debug.hh"

Staff_walker::~Staff_walker() {}
Staff_walker::Staff_walker(Staff_walker const &s)
    :PCursor<Staff_column*> (s)
{
    assert(false);
}

Staff_walker::Staff_walker(Staff * s, PScore*ps )
    : PCursor<Staff_column*> (s->cols)
{
    staff_l_ = s;
    pscore_l_ = ps;
    break_status = BREAK_END - BREAK_PRE;
}

Moment
Staff_walker::when() const
{
    return (* (PCursor<Staff_column*> *) this)->when();
}

void
Staff_walker::process()
{
    break_status = BREAK_END - BREAK_PRE;

    if (ptr()->musical_b()) {
	process_requests();
    } else if (ptr()->staff_commands_p_)
	for (iter_top(*ptr()->staff_commands_p_,i); i.ok(); i++) {
	    process_command(i);
    }
}


void
Staff_walker::process_command(Command*com)
{
    switch (com->code){
    case BREAK_PRE:
    case BREAK_MIDDLE:
    case BREAK_POST:
    case BREAK_END:
	(*this)->score_column_l_->set_breakable();
	break_status = com->code- BREAK_PRE;
	break;
    case INTERPRET:
	do_INTERPRET_command(com);
	break;
	
    case TYPESET:
	do_TYPESET_command(com);
	break;
   
    default :
	break;
    }
}

void
Staff_walker::operator++(int i)
{
    do_pre_move();
    if (ptr()->musical_b() && ptr()->tdescription_
	&& !ptr()->tdescription_->whole_in_measure) {
	*mlog << "[" << ptr()->tdescription_->bars<<"]"<< flush;
    }
    PCursor<Staff_column*>::operator++(i);

    do_post_move();
}
