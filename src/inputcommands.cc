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
    mtor << "Commands_at {";
    tdescription_.print();
    for (iter_top(*this,cc); cc.ok(); cc++) 
	cc->print();
    mtor << "}\n";
#endif
}
Moment
Commands_at::when()
{
    return tdescription_.when;
}
Commands_at::Commands_at(Moment dt, Commands_at* prev)
    : tdescription_(dt, (prev)? &prev->tdescription_ : 0)
{
    if (prev&& !tdescription_.whole_in_measure) {
	bottom().add(get_bar_command());
    }
}

void
Commands_at::add(Input_command *i)
{
    bottom().add(i);

    // should check for other meterchanges here.
    if (i->args[0] == "METER") { 
	int l = i->args[1];
	int o = i->args[2];
	tdescription_.set_meter(l,o);
	bottom().add(get_grouping_command( get_default_grouping(l)));

    }
}

Commands_at::Commands_at(Commands_at const&src) :
    tdescription_(src.tdescription_)
{
    IPointerList<Input_command*> &me(*this);
    const IPointerList<Input_command*> &that(src);
    
    PL_copy(me, that);
}

void
Commands_at::setpartial(Moment p)
{
    tdescription_.setpartial(p);
}

Moment
Commands_at::barleft()
{
    return  tdescription_.barleft();
}

void
Commands_at::parse(Staff_commands_at*s)
{
    s->tdescription_ = tdescription_;
    for (iter_top(*this,cc); cc.ok(); cc++) {
	if (cc->args.sz() &&  cc->args[0] !="") {
	    Command c = **cc;
	    s->add(c);
	    
	}
    }
}
/****************/

void
Input_cursor::find_moment(Moment w)
{
    Moment last = when();
    while  (1) {
	if (! ok() ) {
	    *this = list().bottom();
	    Moment dt = (w - when()) <? ptr()->barleft();

	    Commands_at * c = new Commands_at(dt, *this);
	    assert(c->when() <= w);
	    add(c);
	} else if (when() == w ) {
	    return ;
	} else if (when() > w )
	    break;
	
	
	last = when();
	next();
    }

    prev();
    Moment dt = (w - when());
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
Input_commands::do_skip(int bars, Moment wholes)
{
    while (bars > 0) {
	Moment b = ptr->barleft();
	ptr.find_moment(ptr->when() + b);
	bars --;       	
    }
    if (wholes) {
	ptr.find_moment(ptr->when() + wholes);
    }
}


void
Input_commands::add(Input_command c)
{    
    if (c.args[0] == "PARTIAL") {	
	ptr->setpartial(c.args[1]);
    } else if (c.args[0] == "GROUPING") {
	Input_command *ic = new Input_command(c);
	ic->args.insert(ptr->tdescription_.one_beat, 1);
	ptr->add(ic);
    } else if (c.args[0] == "METER") {
	int beats_per_meas = c.args[1];
	int one_beat = c.args[2];
	Input_command *ch = get_meterchange_command(beats_per_meas, one_beat);
	ptr->add(ch);		
    } else if (c.args[0] == "SKIP") {
	int bars = c.args[1] ;
	Moment wholes= c.args[2];
	do_skip(bars, wholes);
    } else if (c.args[0] == "RESET") {
	ptr= top();
    } else {
	Input_command *ic = new Input_command(c);
	ptr->add(ic);
    } 
    
}

Staff_commands*
Input_commands::parse() const
{
    print();
    Staff_commands*nc = new Staff_commands;

    for (iter_top(*this,i); i.ok(); i++) {

	Staff_commands_at* s= nc->find(i->when());
	if (!s){
	    s = new Staff_commands_at(i->tdescription_);
	    nc->add(s);
	}
	if (!i->when()) {   /* all pieces should start with a breakable. */
	    Command c;//(0.0);
	    c.code = INTERPRET;
	    c.args.add("BAR");
	    c.args.add("empty");
	    s->add(c);
	}

	i->parse(s);
    }
    return nc;
}


void
Input_commands::print() const
{
#ifndef NPRINT
    for (iter_top(*this,cc); cc.ok() ; cc++) {
	cc->print();
    }
#endif
}
/****************/

Moment
Input_cursor::when()const
{
    return (*this)->when(); 
}
Input_cursor::Input_cursor(PCursor<Commands_at *>c)
    : PCursor<Commands_at*>(c)
{
}
