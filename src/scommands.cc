#include "scommands.hh"
#include "debug.hh"
#include "parseconstruct.hh"

/*
  ARG!
  */

/*
  maybe it's time for a "narrowing" cursor?
  */
PCursor<Command*>
Score_commands::first(Real w)
{
    PCursor<Command*> pc(*this);    
    while (pc.ok() && pc->when < w)
	pc++;
    if (!pc.ok() || pc->when != w) {
	Command *c = new Command(w);
	c->priority = 10000;
	if (!pc.ok())
	    pc.add(c);
	else
	    pc.insert(c);
    }

    return pc;
}
/*
  RETURN: pc->when == w && pc.ok
 */

PCursor<Command*>
Score_commands::last_insertion(Real w)
{    
    PCursor<Command*> pc(first(w)), next(pc);    
    while (next.ok() && next->when == w) {
	pc=next;
	next++;
    }
    if (pc->priority != -10000) {
	Command*c = new Command(w);
	c->priority = -10000;
	pc.add(c);
	pc++;
    }
        
    return pc;
}

/*
 */
void
Score_commands::add_seq(svec<Command> com, bool checkbreak)
{
    if (!com.sz())
	return;
    
    Real when = com[0].when;

    PCursor<Command*> begin(first(when));
    PCursor<Command*> end(last_insertion(when));
    if (checkbreak && is_breakable(when)) {
	if (com[0].priority < 0)
	    while (begin->code != BREAK_END)
		begin++;
	else
	    while (end->code != BREAK_PRE)
		end--;
    }
    for (int i = 0; i < com.sz(); i++) {
	insert_between(com[i], begin, end);
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
    k.priority = 5;
    k.code = BREAK_PRE;
    seq.add(k);
    k.priority = 4;
    k.code = BREAK_MIDDLE;
    seq.add(k);
    k.priority = 3;
    k.code = BREAK_POST;
    seq.add(k);
    k.priority = 2;
    k.code = BREAK_END;
    seq.add(k);

    add_seq(seq,false);
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
    PCursor<Command*> c(firstc+1);
    assert(last->when==firstc->when&&firstc < last&&last.ok());
    
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
Score_commands::add_command_to_break(Command pre, Command mid, Command post)
{
    Real w = pre.when;
    assert(w >= 0);
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
    assert(l.ok() && l->when ==w && l->code == BREAK_END);
    
    insert_between(post, f, l);
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
    assert(w >= 0);

    Command pre(w);
    Command mid(w);
    Command post(w);

    if (c.code == INTERPRET)
    {				// UGH
	if (c.args[0] == "BAR") {
	    Command typeset(w);	// kut met peren
	    typeset.code = TYPESET;
	    typeset.args = c.args;
	    typeset.priority = 100;
	    process_add(typeset);
	} else if (c.args[0] == "KEY") {
	    Command typeset(w);
	    typeset.code = TYPESET;
	    typeset.args.add("KEY");
	    typeset.priority = 70;
	    process_add(typeset);
	} else if (c.args[0] == "CLEF") {
	    Command typeset(w);
	    typeset.code = TYPESET;
	    typeset.args=c.args;
	    typeset.priority = 90;
	    process_add(typeset);
	}
    }

    // kut en peer
    if (c.code == TYPESET) {
	if (c.args[0] == "BAR") {
	    set_breakable(w);
	    encapsulate  = true;
	    mid = c;
	    pre = c;
	    { /* every line a currentkey. */
		Command kc(w);
		kc.code =TYPESET;
		kc.args.add( "CURRENTKEY");
		kc.priority = 60;
		process_add(kc);
	    }
	    { /* every line a currentclef. */
		Command kc(w);
		kc.code =TYPESET;
		kc.args.add( "CURRENTCLEF");
		kc.priority = 80;
		process_add(kc);
	    }
	}else
	if (c.args[0] == "METER" && is_breakable(w)) {
	    encapsulate = true;
	    mid = c;
	    pre = c;
	    post =c;
	}else
	if( c.args[0] == "KEY" && is_breakable(c.when)) {
	    encapsulate = true;
	    mid = c;
	    pre = c;
	    post = c;
	}else
	if (c.args[0] == "CURRENTKEY" && is_breakable(w)) {
	    post = c;
	    encapsulate = true;
	}else
	if (c.args[0] == "CURRENTCLEF" && is_breakable(w)) {
	    post = c;
	    encapsulate = true;
	}else
	if (c.args[0] == "CLEF" && is_breakable(w)) {
	    encapsulate = true;
	    post = c;
	    pre = c;
	    mid = c;		       
	}
    }
    
    if (encapsulate)
	add_command_to_break(pre, mid, post);    
    else {
	svec<Command> seq;
	seq.add(c);    
	add_seq(seq,true);
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

/*
  TODO
  */
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

    {   /* all pieces should start with a breakable. */
	Command c(0.0);
	c.code = INTERPRET;
	c.args.add("BAR");
	c.args.add("empty");
	nc->process_add(c);
    }
    for (PCursor<Command*> cc(*this); cc.ok() && cc->when <= l; cc++) {
	assert (cc->code==INTERPRET);
	if (cc->args[0] == "METER") {
	    beats_per_meas = cc->args[1].value();
	    int one_beat = cc->args[2].value();
	    measlen = beats_per_meas/Real(one_beat);
	    nc->process_add(*get_meter_command(wholes, beats_per_meas, one_beat));
	}
	if (cc->args[0] == "KEY"||cc->args[0] == "CLEF") {
	    cc->when = wholes;
	    nc->process_add(**cc);
	}
	if (cc->args[0] == "SKIP") {
	    stoppos = wholes + cc->args[1].value() * measlen +
		cc->args[2].fvalue();
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
