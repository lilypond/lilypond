#include "scommands.hh"
#include "debug.hh"
#include "parseconstruct.hh"

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
Score_commands::insert_between(Command victim, PCursor<Command*> firstc,
			       PCursor<Command*> last)
{
    assert(last->when==firstc->when);
    PCursor<Command*> c(firstc+1);
    while (c != last) {  	// hmm what if !last.ok()?
	if (victim.priority > c->priority) {
	    c.insert(new Command(victim));
	    return;
	}
	c++;
    }
    last.insert(new Command(victim));    
    
}
void
Score_commands::add_command_to_break(Command pre, Command mid,Command post)
{
    Real w = pre.when;
    PCursor<Command*> c ( first(w)), f(c), l(c);

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
    insert_between(post, f, l);
    assert(l.ok() && l->when ==w && l->code == BREAK_END);
}

void
Score_commands::parser_add(Command *c)
{
    bottom().add(c);
}

void
Score_commands::process_add(Command c)
{
    bool encapsulate =false;
    Real w = c.when;
    Command pre(w);
    Command mid(w);
    Command post(w);


    if (c.code == TYPESET) {
	if (c.args[0] == "BAR") {
	    set_breakable(w);
	    encapsulate  = true;
	    mid = c;
	    pre = c;
	}
	if (c.args[0] == "METER" && is_breakable(w)) {
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
	process_add(c);
    }
    
    PCursor<Command*> bot(bottom());

    while (bot.ok() && bot->when > l) {
	mtor <<"removing ";
	bot->print();
	bot.del();
	bot = bottom();
    }

    if (!is_breakable(l)) {
	Command c(l);
	c.code = TYPESET;
	c.args.add("BAR");
	c.args.add("||");
	process_add(c);
    }
    OK();
}

void
Score_commands::OK() const
{
    for (PCursor<Command*> cc(*this); cc.ok() && (cc+1).ok(); cc++) {
	assert(cc->when <= (cc+1)->when);
	if (cc->when == (cc+1)->when && !cc->isbreak() && !(cc+1)->isbreak())
	    assert(cc->priority >= (cc+1)->priority);
    }
}

void
Score_commands::print() const
{
    for (PCursor<Command*> cc(*this); cc.ok() ; cc++) {
	cc->print();
    }
}

Score_commands*
Score_commands::parse(Real l) const
{
    Score_commands*nc = new Score_commands;
    int beats_per_meas=4;
    Real measlen = 1.0; // 4/4 by default
    
    Real inbar=0.0;
    int barcount=0;
    Real wholes=0.0;
    Real stoppos=0.0;

    {
	Command c(0.0);
	c.code = TYPESET;
	c.args.add("BAR");
	c.args.add("empty");
	nc->process_add(c);
    }
    for (PCursor<Command*> cc(*this); cc.ok() && cc->when <= l; cc++) {
	assert (cc->code==INTERPRET);
	if (cc->args[0] == "METER") {
	    beats_per_meas=cc->args[1].value();
	    int one_beat =cc->args[2].value ();
	    measlen = beats_per_meas/Real(one_beat);
	    nc->process_add(*get_meter_command(wholes,beats_per_meas, one_beat));
	}
	if (cc->args[0] == "SKIP") {
	    stoppos = wholes + cc->args[1].value() * measlen + cc->args[2].fvalue();
	    wholes += (measlen-inbar); // skip at least 1 measure
	    barcount++;
	    while (wholes <= stoppos) {
		nc->process_add(*get_bar_command(wholes)); // liek
		wholes += measlen;
		barcount ++;		
	    }
	    wholes = stoppos;
	    //something
	}
    }
    
    return nc;
}
