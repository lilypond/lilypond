#include "string.hh"
#include "parseconstruct.hh"
#include "command.hh"

Command*
get_bar_command(Real w)
{
    Command*c = new Command;
    c->when = w;
    c->code = TYPESET;
    c->args.add( "BAR");
    c->args.add( "|");
    c->priority = 100;
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
    c->priority = 50;		// less than bar
    return c;
}

Command *
get_meterchange_command(int n, int m)
{
    Command*c = new Command;

    c->code = INTERPRET;
    c->args.add( "METER");
    c->args.add( n );
    c->args.add( m );
    c->priority = 0;		// more than bar
    return c;
}


Command *
get_skip_command(int n, Real m)
{
    Command*c = new Command;
    
    c->code = INTERPRET;
    c->args.add( "SKIP");
    c->args.add( n );
    c->args.add( m );
    c->priority = 0;		// more than bar
    return c;
}


