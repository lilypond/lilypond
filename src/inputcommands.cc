/*
  it still sucks.
  */

#include "inputcommands.hh"
#include "inputcommand.hh"
#include "debug.hh"
#include "staffcommands.hh"
#include "getcommand.hh"
#include "command.hh"

void
Commands_at::print() const
{
#ifndef NPRINT
    mtor << "Commands_at { at "<<when<<'\n'; 
    mtor << "meter " << whole_per_measure
	 << " pos "<< bars << ":" << whole_in_measure <<'\n';
    for (PCursor<Input_command *> cc(*this); cc.ok(); cc++) 
	cc->print();
    mtor << "}\n";
#endif
}

Commands_at::Commands_at(Real dt, Commands_at* prev)
{
    if (prev) {
	assert(dt >0);
	when = prev->when + dt;
	whole_per_measure = prev->whole_per_measure;
	whole_in_measure = prev->whole_in_measure + dt;
	bars = prev->bars;

	while ( whole_in_measure >= whole_per_measure ) {
	    whole_in_measure -= whole_per_measure;
	    bars ++;
	}
	if (!whole_in_measure) {
	    bottom().add(get_bar_command());
	}
    } else {
	whole_per_measure = 1;
	whole_in_measure =0;
	when = 0.0;
	bars = 0;
    }
}

void
Commands_at::add(Input_command *i)
{
    bottom().add(i);		
    if (i->args[0] == "METER") { // should check for other meterchanges here.
	Real l = i->args[1];
	Real o = i->args[2];
	whole_per_measure = l/o;		
    }
}

Commands_at::Commands_at(Commands_at const&src)
{
    when = src.when;
    whole_in_measure = whole_in_measure;
    whole_per_measure = whole_per_measure;
    bars = src.bars;
    
    IPointerList<Input_command*> &me(*this);
    const IPointerList<Input_command*> &that(src);
    
    PL_copy(me, that);
}

void
Commands_at::setpartial(Real p)
{
    if (when)
	error_t ("Partial measure only allowed at beginning.", when);
    if (p<0||p > whole_per_measure)
	error_t ("Partial measure has incorrect size", when);
    whole_in_measure = whole_per_measure - p;
}
Real
Commands_at::barleft()
{
    return  whole_per_measure-whole_in_measure;
}

/****************/

void
Input_cursor::find_moment(Real w)
{
    Real last = when();
    while  (1) {
	if (! ok() ) {
	    *this = PCursor<Commands_at*>(list().bottom());
	    Real dt = (w - when()) <? ptr()->barleft();
	    assert(dt >= 0);
	    Commands_at * c = new Commands_at(dt, *this);
	    add(c);	    
	} else if (when() == w ) {
	    return ;
	} else if (when() > w )
	    break;
	
	
	last = when();
	next();
    }

    prev();
    Real dt = (w - when());
    Commands_at * c = new Commands_at(dt, *this);
    add(c);
    next();
}



/****************/
Input_commands::Input_commands(Input_commands const&src)
    : ptr(src.ptr)
{
    IPointerList<Commands_at*> &me(*this);
    const IPointerList<Commands_at*> &that(src);
    
    PL_copy(me, that);    
}

Input_commands::Input_commands()
    :    ptr (bottom())
{
    Commands_at * p = new Commands_at(0,0);    
    bottom().add(p);    
    ptr = bottom();    
}

void
Input_commands::do_skip(int bars, Real wholes)
{
    while (bars > 0) {
	Real b = ptr->barleft();
	ptr.find_moment(ptr->when + b);
	bars --;       	
    }
    if (wholes) {
	ptr.find_moment(ptr->when + wholes);
    }
}


void
Input_commands::add(Input_command c)
{    
    if (c.args[0] == "PARTIAL") {	
	ptr->setpartial(c.args[1]);
    } else if (c.args[0] == "METER") {
	int beats_per_meas = c.args[1];
	int one_beat = c.args[2];
	Input_command *ch = get_meterchange_command(beats_per_meas, one_beat);
	ptr->add(ch);		
    } else if  (c.args[0] == "KEY" || c.args[0] == "CLEF") {
	Input_command *ic = new Input_command(c);
	ptr->add(ic);
    } else if (c.args[0] == "SKIP") {
	int bars = c.args[1] ;
	Real wholes= c.args[2];
	do_skip(bars, wholes);
    } else if (c.args[0] == "RESET") {
	ptr= top();
    }
    
}

Staff_commands*
Input_commands::parse() const
{
    print();
    Staff_commands*nc = new Staff_commands;

    {   /* all pieces should start with a breakable. */
	Command c;//(0.0);
	c.code = INTERPRET;
	c.args.add("BAR");
	c.args.add("empty");
	nc->add(c,0.0);
    }

    for (PCursor<Commands_at*> i(*this); i.ok(); i++)
	for (PCursor<Input_command *> cc(**i); cc.ok(); cc++) {
	    if (cc->args.sz() &&  cc->args[0] !="") {
		Command c = **cc;
//		c.when = i->when;
		nc->add(c, i->when);
	    }
	}
    
    return nc;
}


void
Input_commands::print() const
{
#ifndef NPRINT
    for (PCursor<Commands_at*> cc(*this); cc.ok() ; cc++) {
	cc->print();
    }
#endif
}
/****************/

Real
Input_cursor::when()const
{
    return (*this)->when; 
}
Input_cursor::Input_cursor(PCursor<Commands_at *>c)
    : PCursor<Commands_at*>(c)
{
}
