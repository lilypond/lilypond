#include "inputcommands.hh"
#include "inputcommand.hh"
#include "debug.hh"
#include "staffcommands.hh"
#include "getcommand.hh"
#include "command.hh"
#include "stcol.hh"
#include "staff.hh"
#include "assoc.hh"

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
    if (prev && !tdescription_.whole_in_measure) {
	bottom().add(get_newmeasure_command());
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
	bottom().add(get_grouping_command( get_default_grouping(l,o)));
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
	if (cc->args.size() &&  cc->args[0] !="") {
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
	    
	    Moment dt = (w - when());
	    if ( !ptr()->tdescription_.cadenza_b_ )
		dt = dt <? ptr()->barleft();

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
void
Input_commands::find_moment(Moment m)
{
    ptr.find_moment(m);
}

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
Input_commands::add(Input_command c, Assoc<String,Moment> &marks_assoc_r)
{
    String s(c.args[0]);
    if (s == "CADENZA") {
	ptr->tdescription_.set_cadenza((int)c.args[1]);
    } if (s == "PARTIAL") {	
	ptr->setpartial(c.args[1]);
    } else if (s == "GROUPING") {
	Input_command *ic = new Input_command(c);
	ic->args.insert(ptr->tdescription_.one_beat, 1);
	ptr->add(ic);
    } else if (s == "METER") {
	int beats_per_meas = c.args[1];
	int one_beat = c.args[2];
	Input_command *ch = get_meterchange_command(beats_per_meas, one_beat);
	ptr->add(ch);		
    } else if (s == "SKIP") {
	int bars = c.args[1] ;
	Moment wholes= c.args[2];
	do_skip(bars, wholes);
    } else if (s == "RESET") {
	ptr= top();
    } else if (s=="GOTO") {
	ptr = top();
	String m(c.args[1]);
	if (!marks_assoc_r.elt_query(m))
	    error("Unknown marker: `" +m + "\'");
	
	ptr.find_moment(marks_assoc_r[m]);
    } else {
	Input_command *ic = new Input_command(c);
	ptr->add(ic);
    } 
    
}

void
Input_commands::parse(Staff * staff_l) const
{
    print();
    for (iter_top(*this,i); i.ok(); i++) {

	Staff_column* col_l = staff_l->get_col(i->when(), false);
	if (!col_l->staff_commands_p_)
	    col_l->staff_commands_p_ = new Staff_commands_at(i->tdescription_);
	
	Staff_commands_at * com_l = col_l->staff_commands_p_;
	
	if (!i->when()) {   /* all pieces should start with a breakable. */
	    com_l->set_breakable();
	}

	i->parse(com_l);
    }
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
