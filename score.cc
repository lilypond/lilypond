
#include "tstream.hh"
#include "score.hh"
#include "pscore.hh"
#include "staff.hh"
#include "misc.hh"
#include "debug.hh"

void
Score::add_command_seq(svec<Command *> com)
{
    if (!com.sz())
	return;
    Real when = com[0]->when;

    PCursor<Command*> pc(commands_);    
    while (pc.ok()&&pc->when <= when)
	pc++;
    
    for (int i = 0; i < com.sz(); i++) {
	assert(com[i]->when == when);
	if (!pc.ok())
	    pc.add(com[i]);
	else
	    pc.insert(com[i]);
    }
    
}

void
Score::add(Command *c)
{
    svec<Command*> seq;    
    if (c->code == TYPESET && c->args[0] == "BAR") {
	/* should be encapsulated in BREAKs

	   THIS SUX.

	 */

	Command k;
	
	k.when = c->when;
	k.code = BREAK_PRE;
	
	seq.add(new Command(k));
	seq.add(new Command(*c));
	k.code = BREAK_MIDDLE;
	seq.add(new Command(k));
	seq.add(new Command(*c));
	k.code = BREAK_POST;
	seq.add(new Command(k));
	k.code = BREAK_END;
	seq.add(new Command(k));
    }
    else
	seq.add(new Command(*c));
    
    add_command_seq(seq);
}

void
Score::add(Staff*s)
{
    s->score_ = this;
    staffs_.bottom().add(s);    
}


void
Score::do_pcols()
{
    PCursor<Score_column*> sc(cols_);
    for (;sc.ok(); sc++) {
	pscore_->add(sc->pcol);
    }
}
/*
    this sux. Really makeshift.

    first and last column should be breakable.
    Remove any command past the last musical column.
    */
void
Score::do_miscs()
{
    Command c;

    {
	Command c;
	c.when = 0.0;
	c.code = TYPESET;
	c.args.add("BAR");
	c.args.add("empty");
	add(&c);
    }
    
    PCursor<Command*> bot(commands_.bottom());
    Real l = last();    
    while (bot.ok() && bot->when > l) {

	mtor <<"removing "<< bot->code <<" at " << bot->when<<'\n';
	bot.del();
	bot = commands_.bottom();
    }

    if (bot->when != l || bot->code != BREAK_END) {
	Command c;
	c.code = TYPESET;
	c.when = l;
	c.args.add("BAR");
	c.args.add("||");
	add(&c);
    }
   commands_.OK();
}

Mtime
Score::last() const
{    
    Mtime l = 0;
    for (PCursor<Staff*> stc(staffs_); stc.ok(); stc++) {
	l = MAX(l, stc->last());
    }
    return l;
}
void
Score::clean_commands() 
{
    Mtime l= last();
    for (PCursor<Command*> cc(commands_); cc.ok(); ) {
	if (cc->when > l){
	    mtor << "remming \n";
	    cc.del();
	} else
	    cc++;
    }
}
void
Score::process()
{
     do_miscs();
    
    /// distribute commands to disciples
    distribute_commands();
    
    pscore_ = new PScore;
    for (PCursor<Staff*> sc(staffs_); sc.ok(); sc++) {
	sc->set_output(pscore_);
	sc->process();
    }

    // do this after processing, staffs first have to generate PCols.
    do_pcols();
    calc_idealspacing();
    clean_cols();
    OK();
    //    print();
    pscore_->calc_breaking();
    // TODO: calculate vertical structs
    // TODO: calculate mixed structs.
}

// remove empty cols with no spacing attached.
/* should rethink ownership of cols
    */
void
Score::clean_cols()
{    
    for (PCursor<Staff * > sc(staffs_); sc.ok(); sc++)
	sc->clean_cols();
    
    for (PCursor<Score_column*> c(cols_); c.ok(); ) {
	if (!c->pcol->used) {
	    mtor << "removing : ";
	    c->print();
	    c.del();
	} else
	    c++;
    }
    
    pscore_->clean_cols();
}
/* this sux.  We should have Score_column create the appropriate PCol.
    Unfortunately, PCols don't know about their position.    
    */
// todo
PCursor<Score_column*>
Score::create_cols(Mtime w)
{
    Score_column* c1 = new Score_column(w);
    Score_column* c2 = new Score_column(w);
    
    c1->musical = false;
    c2->musical = true;
    
    PCursor<Score_column*> scc(cols_);

    for (; scc.ok(); scc++) {
	assert(scc->when != w);
	if (scc->when > w)
	    break;
    }

    if (!scc.ok()) {
	cols_.bottom().add(c1);
	cols_.bottom().add(c2);
	scc = cols_.bottom();
	scc --;
    } else {
	scc.insert(c1);
	scc.insert(c2);
	scc -= 2;
    }
    return scc;
}

Score_column*
Score::find_col(Mtime w,bool mus)
{
    PCursor<Score_column*> scc(cols_);
    for (; scc.ok(); scc++) {
	if (scc->when == w && scc->musical == mus)
	    return scc;
	if (scc->when > w)
	    break;
    }
    scc = create_cols(w);
    if (mus)
	scc++;
    return scc;
}

void
Score::distribute_commands(void)
{
    for (PCursor<Staff*> sc(staffs_); sc.ok(); sc++) {
	sc->add_commands(commands_);
    }
}


void
Score::output(String s)
{
    OK();
    if (outfile=="")
	outfile = s;
    
    *mlog << "output to " << outfile << "...\n";
    Tex_stream the_output(outfile);    
    pscore_->output(the_output);
}

void
Score::OK() const
{
#ifndef NDEBUG
    for (PCursor<Staff*> sc(staffs_); sc.ok(); sc++) {
	sc->OK();
	assert(sc->score_ == this);
    }
    staffs_.OK();
    cols_.OK();
    for (PCursor<Score_column*> cc(cols_); cc.ok() && (cc+1).ok(); cc++) {
	assert(cc->when <= (cc+1)->when);
    }
    for (PCursor<Command*> cc(commands_); cc.ok() && (cc+1).ok(); cc++) {
	assert(cc->when <= (cc+1)->when);
    }
#endif    
}

void
Score::print() const
{
#ifndef NPRINT
    mtor << "score {\n"; 
    for (PCursor<Staff*> sc(staffs_); sc.ok(); sc++) {
	sc->print();
    }
    for (PCursor<Score_column*> sc(cols_); sc.ok(); sc++) {
	sc->print();
    }
    mtor << "}\n";
#endif
}
Score::Score()
{
    pscore_=0;
}
/****************************************************************/

Score_column::Score_column(Mtime w)
{
    when = w;
    pcol = new PCol(0);
    musical = false;
}

bool
Score_column::used() {
    return pcol->used;
}

void
Score_column::print() const
{
    #ifndef NPRINT
    mtor << "Score_column { mus "<< musical <<" at " <<  when<<'\n';
    mtor << " # symbols: " << pcol->its.size() << "\n";
    mtor << "durations: [" ;
    for (int i=0; i < durations.sz(); i++)
	mtor << durations[i] << " ";
    mtor << "]\n}\n";
    #endif
}

    
