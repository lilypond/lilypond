#include "staffcommands.hh"
#include "debug.hh"
#include "parseconstruct.hh"
#include "getcommand.hh"

Moment
Staff_commands_at::when()
{
    return tdescription_.when;
}

void
Staff_commands_at::print() const
{
#ifndef NPRINT
    iter_top(*this,i);
    mtor << "Commands at: " ;
    tdescription_.print();
    
    for (; i.ok(); i++)
	i->print();
#endif
}
void
Staff_commands_at::OK()const
{
#ifndef NDEBUG
    iter_top(*this,i);
    for (; i.ok() && (i+1).ok(); i++)
	if (!i->isbreak() && !(i+1)->isbreak())
	    assert(i->priority >= (i+1)->priority);
#endif
}

Staff_commands_at::Staff_commands_at(Time_description m)
    :tdescription_(m)
{
    
}

bool
Staff_commands_at::is_breakable()
{
    iter_top(*this,i);
    for (; i.ok(); i++) {
	if (i->isbreak())
	    return true;
    }
    return false;
}

void
Staff_commands_at::set_breakable()
{
    if (is_breakable()) return;
    
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
    iter_top(*this,c), f(c), l(c);

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
	Command typeset;	// kut met peren
	typeset.code = TYPESET;
	typeset.args = c.args;
	if (c.args[0] == "NEWMEASURE") {
	    add(get_defaultbar_command());
	} else if (c.args[0] == "BAR") {
	    add(typeset);
	    c.code= NOP;	// no INTERPRET (BAR) commands
	} else if (c.args[0] == "KEY") {
	    typeset.priority = 70;
	    add(typeset);
	} else if (c.args[0] == "CLEF") {
	    typeset.priority = 90;
	    add(typeset);
	} else if (c.args[0] == "METER") {
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
	    split_bar_command(pre,mid,post, c.args[1]);

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
	}
	if (is_breakable()) {
	    if (c.args[0] == "METER") {
		mid = c;
		pre = c;
		post =c;
	    }else if( c.args[0] == "KEY") {

		mid = c;
		pre = c;
		post = c;
	    }else if (c.args[0] == "CURRENTKEY" ){
		post = c;

	    }else
		if (c.args[0] == "CURRENTCLEF" ){
		    post = c;

		}else if (c.args[0] == "CLEF") {

		    post = c;
		    pre = c;
		    mid = c;		       
		}
	}
    }
    
    if (encapsulate)
	add_command_to_break(pre, mid, post);
    else {
	if (c.priority>0)
	    top().insert(new Command(c));
	else
	    bottom().add(new Command(c));
    }
}

