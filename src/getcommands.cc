#include "string.hh"
#include "parseconstruct.hh"
#include "command.hh"

Command*
get_partial_command(Real u)
{
    Command*c = new Command;
    c->code = INTERPRET;    
    c->args.add("PARTIAL");
    c->args.add(u);
    return c;
}

Command*
get_reset_command()
{
    Command*c = new Command;
    c->code = INTERPRET;    
    c->args.add("RESET");
    return c;
}

Command*
get_key_interpret_command(svec<String> which)
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
get_clef_interpret_command(String w)
{
    Command*c = new Command;
    c->code = INTERPRET;
    c->args.add("CLEF");
    c->args.add(w);
    c->priority = 190;
    return c;
}

Command*
get_key_typeset_command(svec<String> which)
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
get_meterchange_command(int n, int m)
{
    Command*c = new Command;

    c->code = INTERPRET;
    c->args.add( "METER");
    c->args.add( n );
    c->args.add( m );
    c->priority = 170;		// more than bar
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

Command*
get_bar_command(Real w)
{
    Command*c = new Command;
    c->when = w;
    c->code = INTERPRET;
    c->args.add( "BAR");
    c->args.add( "|");
    c->priority = 170;
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
    c->priority = 0;		
    return c;
}


