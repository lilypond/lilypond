#include "string.hh"
#include "inputcommand.hh"
#include "parseconstruct.hh"
#include "command.hh"

Command*
get_key_interpret_command(svec<Scalar> which)
{
    Command*c = new Command;
    c->code = INTERPRET;    
    c->args= which;
    String k("KEY");
    c->args.insert(k,0 );
    c->priority = 200;
    return c;
}

Command*
get_key_typeset_command(svec<Scalar>which)
{
    Command*c = new Command;
    c->code = TYPESET;    
    c->args = which;
    String k("KEY");
    c->args.insert(k,0 );
    c->priority = 70;
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
    c->priority = 40;
    return c;
}

