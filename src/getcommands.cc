#include "string.hh"
#include "inputcommand.hh"
#include "parseconstruct.hh"
#include "command.hh"

Command*
get_key_typeset_command(Array<Scalar>which)
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
get_meter_command(int n, int m)
{
    Command*c = new Command;
    
    c->code = TYPESET;
    c->args.add( "METER");
    c->args.add( n );
    c->args.add( m );
    c->priority = 40;
    return c;
}

Command
get_defaultbar_command()
{
    Command c;
    c.code = TYPESET;
    c.args.add("BAR");
    c.args.add("default");
    return c;
}

void
split_bar_command(Command &pre_com, Command &no_break_com, Command &post_com,
		  String s)
{
    Command c;
    c.code = TYPESET;
    c.priority = (s=="default") ? 100: 110;
    c.args.add("BAR");
    if(s=="default")
	s = "|";
    
    if (s == "|" || s == ":|" || s == "||") {
	c.args.add(s);
	pre_com= no_break_com= c;	
    } else if (s == ":|:") {
	no_break_com=post_com=pre_com = c;
	
	pre_com.args.add( ":|");
	no_break_com.args.add( s);
	post_com.args.add( "|:");
    }else if (s=="|:") {
	c.args.add(s);
	no_break_com=post_com=c;
    } 
}
