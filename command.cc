
#include "string.hh"
#include "command.hh"


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
