#include "tstream.hh"
#include "score.hh"
#include "sccol.hh"
#include "pscore.hh"
#include "staff.hh"
#include "debug.hh"
#include "paper.hh"


void
Score::process()
{
    *mlog << "Processing ... ";
    
    assert (paper_);
    
    /// distribute commands to disciples
    pscore_ = new PScore(paper_);
    for (PCursor<Staff*> i(staffs_); i.ok(); i++) {
	i->process_commands(last());
	i->set_output(pscore_);
	i->process();
    }

    // do this after processing, staffs first have to generate PCols.
    do_pcols();
    // ugh. Would want to clean the columns before anything else.
    clean_cols();
    calc_idealspacing();

    // debugging
    print ();
    OK();

    pscore_->preprocess();
    *mlog << "Calculating ... ";
    pscore_->calc_breaking();
    *mlog << "Postprocessing ... ";
    pscore_->postprocess();

    // TODO: calculate vertical structs?
    // TODO: calculate mixed structs.?
    *mlog << "\n";
}

// remove empty cols.
void
Score::clean_cols()
{    
    for (PCursor<Staff * > i(staffs_); i.ok(); i++)
	i->clean_cols();
    
    for (PCursor<Score_column*> c(cols_); c.ok(); ) {
	if (!c->pcol_->used()) {
	    c.del();
	} else {
	    c->preprocess();
	    c++;
	}
    }
    
    pscore_->clean_cols();
}
/*
  this sux.  We should have Score_column create the appropriate PCol.
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

PCursor<Score_column*>
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
Score::do_pcols()
{
    PCursor<Score_column*> i(cols_);
    for (; i.ok(); i++) {
	pscore_->add(i->pcol_);
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
    for (PCursor<Staff*> i(staffs_); i.ok(); i++) {
	i->OK();
	assert(i->score_ == this);
    }
    staffs_.OK();
    cols_.OK();
    for (PCursor<Score_column*> cc(cols_); cc.ok() && (cc+1).ok(); cc++) {
	assert(cc->when <= (cc+1)->when);
    }
#endif    
}


void
Score::print() const
{
#ifndef NPRINT
    mtor << "score {\n"; 
    for (PCursor<Staff*> i(staffs_); i.ok(); i++) {
	i->print();
    }
    for (PCursor<Score_column*> i(cols_); i.ok(); i++) {
	i->print();
    }
    if (pscore_)
	pscore_->print();
    
    mtor << "}\n";
#endif
}

Score::Score(Paperdef*p)
{
    pscore_=0;
    paper_ = p;
}

Score::~Score()
{
    delete pscore_;
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
Score::add(Staff*s)
{
    s->score_ = this;
    staffs_.bottom().add(s);
}

