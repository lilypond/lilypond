#include "inputcommand.hh"
#include "debug.hh"
#include "command.hh"

Input_command::Input_command()
{
}


Input_command::operator Command()
{
    Command c;
    if (!args.size())
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
get_partial_command(Moment u)
{
    Input_command*c = new Input_command;
    c->args.push("PARTIAL");
    c->args.push(u);
    return c;
}

Input_command*
get_goto_command(String s)
{
    Input_command*c = new Input_command;
    c->args.push("GOTO");
    c->args.push(s);
    return c;
}

Input_command*
get_cadenza_toggle(int i)
{
    
    Input_command*c = new Input_command;
    c->args.push("CADENZA");
    c->args.push(i);
    return c;
}
Input_command*
get_grouping_command(Array<int>a ) 
{
    Input_command*c = new Input_command;
    c->args.push("GROUPING");    
    for (int i=0; i < a.size(); i ++)
	c->args.push(a[i]);

    return c;
}

Input_command*
get_key_interpret_command(Array<int >a ) 
{
    Input_command*c = new Input_command;
    c->args.push("KEY");
    for (int i=0; i < a.size(); i ++) {
	c->args.push(a[i]);
    }
    return c;
}

Input_command*
get_reset_command()
{
    Input_command*c = new Input_command;
    c->args.push("RESET");
    return c;
}

Input_command *
get_meterchange_command(int n, int m)
{
    Input_command*c = new Input_command;

    c->args.push( "METER");
    c->args.push( n );
    c->args.push( m );

    return c;
}

Input_command *
get_newmeasure_command()
{
    Input_command*c = new Input_command;
    c->args.push( "NEWMEASURE");
    return c;
}

Input_command *
get_skip_command(int n, Moment m)
{
    Input_command*c = new Input_command;
    
    c->args.push( "SKIP");
    c->args.push( n );
    c->args.push( m );

    return c;
}


void
Input_command::print()const
{
#ifndef NPRINT
    mtor << "{ ";
    if (args.size()) {
	mtor<< " args: ";
	for (int i = 0; i<args.size(); i++)
	    mtor << "`"<<args[i] <<"',";
    }
    mtor << "}\n";
#endif    
}

Input_command*
get_clef_interpret_command(String w)
{
    Input_command*c = new Input_command;
    c->args.push("CLEF");
    c->args.push(w);
    return c;
}

Input_command*
get_bar_command(String w)
{
    Input_command*c = new Input_command;
    c->args.push("BAR");
    c->args.push(w);
    return c;
}

Array<int>
get_default_grouping(int count)
{
    Array<int> s;
    if (!(count % 3 )) {
	for (int i=0; i < count/3; i++)
	    s.push(3);
    } else if (!(count %2)) {
	for (int i=0; i < count/2; i++)
	    s.push(2);
    }else {
	s.push(2);
	s.concat(get_default_grouping(count-2));
    }
    return s;
}
