#include "string.hh"
#include "debug.hh"
#include "command.hh"

bool
Command::isbreak()const
{
    return (code >= BREAK_PRE&&code <= BREAK_END);
}

Command*
get_bar_command(Real w)
{
    Command*c = new Command;
    c->when = w;
    c->code = TYPESET;
    c->args.add( "BAR");
    c->args.add( "|");
    return c;
}

Command *
get_meter_command(Real w, int n, int m)
{
    Command*c = new Command;
    
    c->when = w;
    c->code = TYPESET;
    c->args.add( "METER");
    c->args.add( n );
    c->args.add( m );
    return c;
}



Command::Command()
{
    code = NOP;
    when = -1;
}



Command::Command(Real w)
{
    code = NOP;
    when = w;
}
void
Command::print() const
{
    mtor << "command code: " << code << " args: ";
    for (int i = 0; i<args.sz(); i++)
	mtor << args[i];
    mtor << "\n";
}
