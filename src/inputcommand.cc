#include "inputcommand.hh"
#include "debug.hh"
#include "command.hh"

Input_command::Input_command()
{
    when = -1;
}

Input_command::Input_command(Real w)
{
    when =w;
}

Input_command::operator Command()
{
    Command c(when);
    if (!args.sz())
	return c;
    
    c.code = INTERPRET;
    String s = args[0];
        
    int p=0;
    if (s == "KEY")
	p = 200;
    else if (s=="CLEF")
	p = 190;
    else if (s == "METER")
	p = 180;
    else if (s == "BAR")
	p = 170;
    else if (s == "GROUPING")
	p = 160;
    
    c.priority = p;
    c.args = args;
    
    return c;    
}


Input_command*
get_partial_command(Real u)
{
    Input_command*c = new Input_command;
    c->args.add("PARTIAL");
    c->args.add(u);
    return c;
}

Input_command*
get_grouping_command(svec<int>a ) 
{
    Input_command*c = new Input_command;
    c->args.add("GROUPING");
    for (int i=0; i < a.sz(); i ++)
	c->args.add(a[i]);

    return c;
}

Input_command*
get_key_interpret_command(svec<int >a ) 
{
    Input_command*c = new Input_command;
    c->args.add("KEY");
    for (int i=0; i < a.sz(); i ++) {
	c->args.add(a[i]);
    }
    return c;
}

Input_command*
get_reset_command()
{
    Input_command*c = new Input_command;
    c->args.add("RESET");
    return c;
}

Input_command *
get_meterchange_command(int n, int m)
{
    Input_command*c = new Input_command;

    c->args.add( "METER");
    c->args.add( n );
    c->args.add( m );

    return c;
}

Input_command *
get_bar_command(Real w)
{
    Input_command*c = new Input_command;
    c->when = w;
    c->args.add( "BAR");
    c->args.add( "|");

    return c;
}

Input_command *
get_skip_command(int n, Real m)
{
    Input_command*c = new Input_command;
    
    c->args.add( "SKIP");
    c->args.add( n );
    c->args.add( m );

    return c;
}


void
Input_command::print()const
{
    mtor << "{ at " << when;
    if (args.sz()) {
	mtor<< " args: ";
	for (int i = 0; i<args.sz(); i++)
	    mtor << "`"<<args[i] <<"',";
    }
    mtor << "}\n";
}

Input_command*
get_clef_interpret_command(String w)
{
    Input_command*c = new Input_command;
    c->args.add("CLEF");
    c->args.add(w);
    return c;
}
