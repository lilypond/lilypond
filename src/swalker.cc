#include "request.hh"
#include "swalker.hh"
#include "stcol.hh"

Staff_walker::~Staff_walker() {}

Staff_walker::Staff_walker(Staff * s, PScore*ps )
    : PCursor<Staff_column*> (s->cols)
{
    staff_ = s;
    pscore_ = ps;
    break_status = BREAK_END - BREAK_PRE;
}

Real
Staff_walker::when() const
{
    return (* (PCursor<Staff_column*> *) this)->when();
}

void
Staff_walker::process()
{
    break_status = BREAK_END - BREAK_PRE;

    for (int i = 0 ; i < (*this)->s_commands.sz(); i++) {
	process_command((*this)->s_commands[i]);
    }

    process_requests();
}

