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
    c->args.add("PARTIAL");
    c->args.add(u);
    return c;
}

Input_command*
get_goto_command(String s)
{
    Input_command*c = new Input_command;
    c->args.add("GOTO");
    c->args.add(s);
    return c;
}

Input_command*
get_cadenza_toggle(int i)
{
    
    Input_command*c = new Input_command;
    c->args.add("CADENZA");
    c->args.add(i);
    return c;
}
Input_command*
get_grouping_command(Array<int>a ) 
{
    Input_command*c = new Input_command;
    c->args.add("GROUPING");    
    for (int i=0; i < a.size(); i ++)
	c->args.add(a[i]);

    return c;
}

Input_command*
get_key_interpret_command(Array<int >a ) 
{
    Input_command*c = new Input_command;
    c->args.add("KEY");
    for (int i=0; i < a.size(); i ++) {
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
get_newmeasure_command()
{
    Input_command*c = new Input_command;
    c->args.add( "NEWMEASURE");
    return c;
}

Input_command *
get_skip_command(int n, Moment m)
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
    c->args.add("CLEF");
    c->args.add(w);
    return c;
}

Input_command*
get_bar_command(String w)
{
    Input_command*c = new Input_command;
    c->args.add("BAR");
    c->args.add(w);
    return c;
}

Array<int>
get_default_grouping(int count)
{
    Array<int> s;
    if (!(count % 3 )) {
	for (int i=0; i < count/3; i++)
	    s.add(3);
    } else if (!(count %2)) {
	for (int i=0; i < count/2; i++)
	    s.add(2);
    }else {
	s.add(2);
	s.concat(get_default_grouping(count-2));
    }
    return s;
}
