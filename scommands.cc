#include "scommands.hh"
#include "debug.hh"

/*
  maybe it's time for a "narrowing" cursor?
  */
PCursor<Command*>
Score_commands::first(Real w)
{
    PCursor<Command*> pc(*this);    
    while (pc.ok() && pc->when < w)
	pc++;
    
     return pc;
}

PCursor<Command*>
Score_commands::last_insertion(Real w)
{    
    PCursor<Command*> pc(*this);    
    while (pc.ok() && pc->when <= w)
	pc++;
    return pc;
}

void
Score_commands::add_seq(svec<Command> com)
{
    if (!com.sz())
	return;
    Real when = com[0].when;

    PCursor<Command*> pc(last_insertion(when));
    for (int i = 0; i < com.sz(); i++) {
	Command *c = new Command(com[i]);
	assert(com[i].when == when);
	if (!pc.ok())
	    pc.add(c);
	else
	    pc.insert(c);
    }
}

void
Score_commands::set_breakable(Real when)
{
    bool found_typeset(false);
    PCursor<Command*> cc = first(when);
    for (; cc.ok() && cc->when == when; cc++) {
	if (cc->isbreak())
	    return;
	if (cc->code == TYPESET)
	    found_typeset=true;
    }

    assert(!found_typeset);
    
    svec<Command> seq;
    Command k(when);
    k.code = BREAK_PRE;
    seq.add(k);
    k.code = BREAK_MIDDLE;
    seq.add(k);
    k.code = BREAK_POST;
    seq.add(k);
    k.code = BREAK_END;
    seq.add(k);

    add_seq(seq);
}
bool
Score_commands::is_breakable(Real w)
{
    PCursor<Command*> cc = first(w);
    for (; cc.ok() && cc->when == w; cc++) {
	if (cc->isbreak())
	    return true;
    }
    return false;
}
void
Score_commands::add_command_to_break(Command pre, Command mid,Command post)
{
    Real w = pre.when;
    
    Command k(w);
    
    PCursor<Command*> c ( first(w));
    while (!c->isbreak())
	c++;
    c.add(new Command(pre));

    while (!c->isbreak())
	c++;
    c.add(new Command(mid));

    while (!c->isbreak())
	c++;
    c.add(new Command(post));
}

void
Score_commands::add(Command c)
{
    bool encapsulate =false;

    Command pre(c.when);
    Command mid(c.when);
    Command post(c.when);


    if (c.code == TYPESET) {
	if (c.args[0] == "BAR") {
	    set_breakable(c.when);
	    encapsulate  = true;
	    mid = c;
	    pre = c;
	}
	if (c.args[0] == "METER" && is_breakable(c.when)) {
	    encapsulate = true;
	    mid = c;
	    pre = c;
	    post =c;
	}
    }
    
    if (encapsulate)
	add_command_to_break(pre, mid, post);    
    else {
	svec<Command> seq;
	seq.add(c);    
	add_seq(seq);
    }
}

/*
    first and last column should be breakable.
    Remove any command past the last musical column.
    */
void
Score_commands::clean(Real l)
{
    assert(l>0);
    if (!is_breakable(0.0)) {
	Command c(0.0);
	c.code = TYPESET;
	c.args.add("BAR");
	c.args.add("empty");
	add(c);
    }
    
    PCursor<Command*> bot(bottom());

    while (bot.ok() && bot->when > l) {

	mtor <<"removing "<< bot->code <<" at " << bot->when<<'\n';
	bot.del();
	bot = bottom();
    }

    if (!is_breakable(l)) {
	Command c(l);
	c.code = TYPESET;
	c.args.add("BAR");
	c.args.add("||");
	add(c);
    }
    OK();
}

void
Score_commands::OK() const
{
    for (PCursor<Command*> cc(*this); cc.ok() && (cc+1).ok(); cc++) {
	assert(cc->when <= (cc+1)->when);
    }
}

void
Score_commands::print() const
{
    for (PCursor<Command*> cc(*this); cc.ok() ; cc++) {
	cc->print();
    }
}
