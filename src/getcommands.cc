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
    c->args.push( "METER");
    c->args.push( n );
    c->args.push( m );
    c->priority = 40;
    return c;
}

Command
get_defaultbar_command()
{
    Command c;
    c.code = TYPESET;
    c.args.push("BAR");
    c.args.push("default");
    return c;
}

void
split_bar_command(Command &pre_com, Command &no_break_com, Command &post_com,
		  String s)
{
    Command c;
    c.code = TYPESET;
    c.priority = (s=="default") ? 100: 110;
    c.args.push("BAR");
    if(s=="default")
	s = "|";
    
    if (s == ":|:") {
	no_break_com=post_com=pre_com = c;
	
	pre_com.args.push( ":|");
	no_break_com.args.push( s);
	post_com.args.push( "|:");
    }else if (s=="|:") {
	c.args.push(s);
	no_break_com=post_com=c;
    } else {
	c.args.push(s);
	pre_com= no_break_com= c;	
    } 
}
