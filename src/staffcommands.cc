#include "staffcommands.hh"
#include "debug.hh"
#include "parseconstruct.hh"

Moment
Staff_commands_at::when()
{
    return tdescription_.when;
}
void
Staff_commands_at::print() const
{
#ifndef NPRINT
    PCursor<Command*> i (*this);
    mtor << "Commands at: " ;
    tdescription_.print();
    
    for (; i.ok(); i++)
	i->print();
#endif
}
void
Staff_commands_at::OK()const
{
    PCursor<Command*> i (*this);
    for (; i.ok() && (i+1).ok(); i++)
	if (!i->isbreak() && !(i+1)->isbreak())
	    assert(i->priority >= (i+1)->priority);
}

Staff_commands_at::Staff_commands_at(Time_description m)
    :tdescription_(m)
{
    
}

bool
Staff_commands_at::is_breakable()
{
    PCursor<Command*> i(*this);
    for (; i.ok(); i++) {
	if (i->isbreak())
	    return true;
    }
    return false;
}

void
Staff_commands_at::set_breakable()
{
    assert(!is_breakable());
    
    Command k;
    k.code = BREAK_PRE;
    bottom().add(new Command(k));
    k.code = BREAK_MIDDLE;
    bottom().add(new Command(k));
    k.code = BREAK_POST;
    bottom().add(new Command(k));
    k.code = BREAK_END;
    bottom().add(new Command(k));        
}

void
Staff_commands_at::insert_between(Command victim, PCursor<Command*> firstc,
				  PCursor<Command*> last)
{
    PCursor<Command*> c(firstc+1);
    assert(firstc < last&&last.ok());
    
    while (c < last) { 
	if (c->priority <= victim.priority) {
	    c.insert(new Command(victim));
	    return;
	}
	c++;
    }
    last.insert(new Command(victim));    
}

void
Staff_commands_at::add_command_to_break(Command pre, Command mid,Command post)
{
    assert(is_breakable());
    PCursor<Command*> c ( *this), f(c), l(c);

    while (!c->isbreak())
	c++;
    f = c++;
    while (!c->isbreak())
	c++;
    l = c++;
    
    insert_between(pre, f, l);
    f = l;
    while (!c->isbreak())
	c++;
    l = c++;    
    insert_between(mid, f, l);
    f = l;
    while (!c->isbreak())
	c++;
    l = c++;
    assert(l.ok() && l->code == BREAK_END);
    
    insert_between(post, f, l);
}


/*
  should move this stuff into inputlanguage.
  */
void
Staff_commands_at::add(Command c)
{
    bool encapsulate =false;
    Command pre;
    Command mid;
    Command post;

    if (c.code == INTERPRET)
    {				// UGH
	if (c.args[0] == "BAR") {
	    Command typeset;	// kut met peren
	    typeset.code = TYPESET;
	    typeset.args = c.args;
	    typeset.priority = 100;
	    add(typeset);
	} else if (c.args[0] == "KEY") {
	    Command typeset;
	    typeset.code = TYPESET;
	    typeset.args.add("KEY");
	    typeset.priority = 70;
	    add(typeset);
	} else if (c.args[0] == "CLEF") {
	    Command typeset;
	    typeset.code = TYPESET;
	    typeset.args=c.args;
	    typeset.priority = 90;
	    add(typeset);
	} else if (c.args[0] == "METER") {
	    Command typeset;
	    typeset.code = TYPESET;
	    typeset.args=c.args;
	    typeset.priority = 40;
	    add(typeset);
	    return;
	}
    }

    // kut en peer
    if (c.code == TYPESET) {
	encapsulate  = is_breakable();
	if (c.args[0] == "BAR") {
	    set_breakable();
	    encapsulate = true;
	    mid = c;
	    pre = c;
	    { /* every line a currentkey. */
		Command kc;
		kc.code =TYPESET;
		kc.args.add( "CURRENTKEY");
		kc.priority = 60;
		add(kc);
	    }
	    { /* every line a currentclef. */
		Command kc;
		kc.code =TYPESET;
		kc.args.add( "CURRENTCLEF");
		kc.priority = 80;
		add(kc);
	    }
	}else if (c.args[0] == "METER" && is_breakable()) {
	    mid = c;
	    pre = c;
	    post =c;
	}else
	if( c.args[0] == "KEY" && is_breakable()) {

	    mid = c;
	    pre = c;
	    post = c;
	}else if (c.args[0] == "CURRENTKEY" && is_breakable() ){
	    post = c;

	}else
	if (c.args[0] == "CURRENTCLEF" && is_breakable() ){
	    post = c;

	}else
	if (c.args[0] == "CLEF" && is_breakable()) {

	    post = c;
	    pre = c;
	    mid = c;		       
	}
    }
    
    if (encapsulate)
	add_command_to_break(pre, mid, post);
    else {
	bottom().add(new Command(c));
    }
}


/****************************************************************/

void
Staff_commands::OK() const
{
#ifndef NDEBUG
    for (PCursor<Staff_commands_at*> i(*this); i.ok() && (i+1).ok(); i++) {
	assert(i->tdescription_.when <= (i+1)->tdescription_.when);
	i->OK();
    }
#endif
}

void
Staff_commands::print() const
{
#ifndef NPRINT
    for (PCursor<Staff_commands_at*> i(*this); i.ok() ; i++) {
	i->print();
    }
#endif
}

Staff_commands_at*
Staff_commands::find(Moment w)
{
    PCursor<Staff_commands_at*> i(bottom());
    for (; i.ok() ; i--) {
	if (i->tdescription_.when == w)
	    return i;
	if (i->tdescription_.when < w)
	    break;
    }
    return 0;
}

void
Staff_commands::add(Staff_commands_at*p)
{
    PCursor<Staff_commands_at*> i(bottom());
    for (; i.ok() ; i--) {
	if (i->tdescription_.when < p->tdescription_.when)
	    break;
    }
    if (!i.ok()) 
	i.insert(p);
    else {
	i.add(p);
	i++;
    }
}

void
Staff_commands::clean(Moment l)
{
    PCursor<Staff_commands_at*> i(bottom());
    for (; i->tdescription_.when > l; i=bottom()) {
	remove(i);
    }
    
    Staff_commands_at*p = find(l);
    if (!p) {
	p = new Staff_commands_at(Time_description(l - i->when(), &i->tdescription_));
	add(p);
    }
    if (!p->is_breakable()) {
	p->set_breakable();
    }
}
