#include "string.hh"
#include "debug.hh"
#include "command.hh"

bool
Command::isbreak()const
{
    return (code >= BREAK_PRE && code <= BREAK_END);
}

Command::Command()
{
    code = NOP;
    when = -1;
    priority=0;
}



Command::Command(Real w)
{
    code = NOP;
    when = w;
    priority=0;
}

void
Command::print() const
{
#ifndef NPRINT
    mtor << "command at " << when << ", code " << code << " prio " << priority;
    if (args.sz()) {
	mtor<< " args: ";
	for (int i = 0; i<args.sz(); i++)
	    mtor << "`"<<args[i] <<"',";
    }
    mtor << "\n";
#endif
}
