#include "scommands.hh"
#include "tstream.hh"
#include "score.hh"
#include "sccol.hh"
#include "pscore.hh"
#include "staff.hh"
#include "debug.hh"
#include "paper.hh"

void
Score::set(Paperdef*p)
{
    delete paper_;
    paper_ = p;
}

void
Score::output(String s)
{
    OK();
    if (paper_->outfile=="")
	paper_->outfile = s;
    
    *mlog << "output to " << paper_->outfile << "...\n";
    Tex_stream the_output(paper_->outfile);    
    pscore_->output(the_output);
}


void
Score::process()
{
    *mlog << "Processing ... ";
    set(commands_->parse(last()));
    commands_->print();
    
    if (!paper_)
	paper_ = new Paperdef;
    
    commands_->clean(last());
    
    /// distribute commands to disciples
    distribute_commands();
    
    pscore_ = new PScore(paper_);
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

    pscore_->preprocess();
    *mlog << "Calculating ... ";
    pscore_->calc_breaking();
    pscore_->postprocess();

    // TODO: calculate vertical structs
    // TODO: calculate mixed structs.
    *mlog << "\n";
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
	if (!c->pcol_->used) {
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
Score::create_cols(Real w)
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
Score::find_col(Real w,bool mus)
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
	sc->add_commands(*commands_);
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
	pscore_->add(sc->pcol_);
    }
}
Real
Score::last() const
{    
    Real l = 0;
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
    commands_->OK();
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
    commands_->print();
    mtor << "}\n";
#endif
}

Score::Score()
{
    pscore_=0;
    paper_ = 0;
    commands_ = new Score_commands;
}

Score::~Score()
{
    delete pscore_;
    delete commands_;
    delete paper_;
}

void
Score::set(Score_commands*c)
{
    delete commands_;
    commands_ = c;
}
