#include "tstream.hh"
#include "score.hh"
#include "sccol.hh"
#include "pscore.hh"
#include "staff.hh"
#include "debug.hh"
#include "paper.hh"

void
Score::output(String s)
{
    OK();
    if (paper->outfile=="")
	paper->outfile = s;
    
    *mlog << "output to " << paper->outfile << "...\n";
    Tex_stream the_output(paper->outfile);    
    pscore_->output(the_output);
}


void
Score::process()
{
    if (!paper)
	paper = new Paperdef;

    commands_.clean(last());
    
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
Score::add(Staff*s)
{
    s->score_ = this;
    staffs_.bottom().add(s);    
}


void
Score::do_pcols()
{
    PCursor<Score_column*> sc(cols_);
    for (; sc.ok(); sc++) {
	pscore_->add(sc->pcol);
    }
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
    commands_.OK();
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
    commands_.print();
    mtor << "}\n";
#endif
}

Score::Score()
{
    pscore_=0;
    paper = 0;
}
void
Score::add(Command*c)
{
    commands_.add(*c);
}
