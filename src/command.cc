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
    priority=0;
}

void
Command::print() const
{
#ifndef NPRINT
    mtor << "Command " << "code " << code << " prio " << priority;
    if ( isbreak())
	mtor << "(break separator)";
    if (args.sz()) {
	mtor<< " args: ";
	for (int i = 0; i<args.sz(); i++)
	    mtor << "`"<<args[i] <<"',";
    }
    mtor << "\n";
#endif
}
