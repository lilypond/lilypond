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
    *mlog << "\nProcessing ... ";
    
    assert (paper_);
    
    /// distribute commands to disciples
    pscore_ = new PScore(paper_);
    for (iter_top(staffs_,i); i.ok(); i++) {
	i->process_commands(last());
	i->set_output(pscore_);
	i->process();
    }

    // do this after processing, staffs first have to generate PCols.
    do_pcols();

    clean_cols();    // can't move this farther up.

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
    for (iter_top(staffs_,i); i.ok(); i++)
	i->clean_cols();
    
    for (iter_top(cols_,c); c.ok(); ) {
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
Score::create_cols(Moment w)
{
    Score_column* c1 = new Score_column(w);
    Score_column* c2 = new Score_column(w);
    
    c1->musical = false;
    c2->musical = true;
    
    iter_top(cols_,i);

    for (; i.ok(); i++) {
	assert(i->when != w);
	if (i->when > w)
	    break;
    }

    if (!i.ok()) {
	cols_.bottom().add(c1);
	cols_.bottom().add(c2);
	i = cols_.bottom();
	i --;
    } else {
	i.insert(c1);
	i.insert(c2);
	i -= 2;
    }
    return i;
}

PCursor<Score_column*>
Score::find_col(Moment w,bool mus)
{
    iter_top(cols_,i);
    for (; i.ok(); i++) {
	if (i->when == w && i->musical == mus)
	    return i;
	if (i->when > w)
	    break;
    }
    i = create_cols(w);
    if (mus)
	i++;
    return i;
}

void
Score::do_pcols()
{
    iter_top(cols_,i);
    for (; i.ok(); i++) {
	pscore_->add(i->pcol_);
    }
}
Moment
Score::last() const
{    
    Moment l = 0;
    for (iter_top(staffs_,i); i.ok(); i++) {
	l = l>? i->last();
    }
    return l;
}

void
Score::OK() const
{
#ifndef NDEBUG
    for (iter_top(staffs_,i); i.ok(); i++) {
	i->OK();
	assert(i->score_ == this);
    }
    staffs_.OK();
    cols_.OK();
    for (iter_top(cols_,cc); cc.ok() && (cc+1).ok(); cc++) {
	assert(cc->when <= (cc+1)->when);
    }
#endif    
}


void
Score::print() const
{
#ifndef NPRINT
    mtor << "score {\n"; 
    for (iter_top(staffs_,i); i.ok(); i++) {
	i->print();
    }
    for (iter_top(cols_,i); i.ok(); i++) {
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

