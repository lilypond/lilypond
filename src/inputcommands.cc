/*
  it still sucks.
  */

#include "inputcommands.hh"
#include "debug.hh"
#include "staffcommands.hh"
#include "getcommand.hh"
#include "command.hh"

Input_commands::Input_commands(Input_commands const&src)
    : ptr(src.ptr)
{
    IPointerList<Command*> &me(*this);
    const IPointerList<Command*> &that(src);
    
    PL_copy(me, that);    
}

Input_commands::Input_commands()
    :    ptr (bottom())
{
    Command c(0.0);		
    bottom().add(new Command(c));    
    ptr = bottom();    
}

void
Input_commands::truncate(Real last)
{
    bool reset_ = false;
    
    if (ptr.when() >= last)
	reset_=true;
    PCursor<Command*> i(*this);
    
    while (i.ok() && i ->when < last)
	i++;

    while (i.ok())
	i.del();

    if (reset_) {
	reset();
	
	while(ptr.ok())
	    ptr++;	
    }
}

/*
  ugh. This sux.
  */
void
Input_commands::find_moment(Real w)
{
    assert(w >= ptr.when());
    while (ptr.ok() && ptr.when() < w) {
	ptr++;
    }
    
    if (!ptr.ok()) {
	int bars_left =int(floor( (w - ptr.last + ptr.whole_in_measure)
				  /ptr.whole_per_measure));
	if (bars_left) {
	    Real bar_when = ptr.last - ptr.whole_in_measure + ptr.whole_per_measure;
	    ptr.addbot(get_bar_command(bar_when));
	    find_moment(w);	// tail-recursion. todo
	} else {
	    ptr.addbot(new Command(w));
	}

    } else if (ptr.when() != w) {
	ptr.insert(new Command(w));
	ptr--;	
    }
}

void
Input_commands::do_skip(int & bars, Real & wholes)
{
    if (wholes) {
	find_moment(ptr.when() +wholes);
	wholes = 0.0;	
    }

    if (bars) {
	ptr.last_command_here(); // find any METER change commands.
	if (ptr.whole_in_measure){
	    Real barleft = ptr.whole_per_measure - ptr.whole_in_measure;
	    do_skip(bars, barleft);
	}else {
	    find_moment(ptr.when() + bars*ptr.whole_per_measure);
	    bars = 0;
	}
    }
}


void
Input_commands::add(Command *c)
{    
    assert(c->code==INTERPRET);
    if (c->args[0] == "PARTIAL") {
	Real p = c->args[1].fvalue();
	ptr.setpartial(p);
	
    } else if (c->args[0] == "METER") {
	int beats_per_meas, one_beat;
	Real r;
	
	interpret_meter(c, beats_per_meas, one_beat, r);
	Command *ch = get_meterchange_command(beats_per_meas, one_beat);
	ch->when = ptr.when();	
	ptr.add(ch);
	
	delete c;
	
    } else if  (c->args[0] == "KEY" || c->args[0] == "CLEF") {
	c->when = ptr.when();
	ptr.add(c);
    } else if (c->args[0] == "SKIP") {
	int bars = c->args[1].value() ;
	Real wholes= c->args[2].fvalue();
	while (bars > 0 || wholes > 0.0) {
	    do_skip(bars, wholes);
	}
	delete c;
    } else if (c->args[0] == "RESET") {
	delete c;
	
	reset();	
    }
    
}

void
Input_commands::reset()
{
    ptr = top();
    ptr.reset();
}


Staff_commands*
Input_commands::parse() const
{
    Staff_commands*nc = new Staff_commands;

    {   /* all pieces should start with a breakable. */
	Command c(0.0);
	c.code = INTERPRET;
	c.args.add("BAR");
	c.args.add("empty");
	nc->process_add(c);
    }

    for (PCursor<Command*> cc(*this); cc.ok(); cc++) {
	if (cc->code != NOP)
	    nc->process_add(**cc);
    }
    
    return nc;
}


void
Input_commands::print() const
{
#ifndef NPRINT
    for (PCursor<Command*> cc(*this); cc.ok() ; cc++) {
	cc->print();
    }
    ptr.print();
#endif
}
