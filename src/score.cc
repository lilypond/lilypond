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
    *mlog << "\nProcessing music ... ";
    
    assert (paper_p_);
    
    /// distribute commands to disciples
    pscore_p_ = new PScore(paper_p_);
    for (iter_top(staffs_,i); i.ok(); i++) {
	i->truncate_cols(last());
	i->set_output(pscore_p_);
	i->process();
    }

    // do this after processing, staffs first have to generate PCols.
    find_col(last(), false)->set_breakable();
    do_cols();
    calc_idealspacing();

    // debugging
    OK();
    pscore_p_->process();    
    *mlog << "\n";
}

// remove empty cols.
void
Score::clean_cols()
{    
    for (iter_top(staffs_,i); i.ok(); i++)
	i->clean_cols();
    
    for (iter_top(cols_,c); c.ok(); ) {
	if (!c->pcol_l_->used()) {
	    c.del();
	} else {
	    c->preprocess();
	    c++;
	}
    }
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
    
    c1->musical_ = false;
    c2->musical_ = true;
    
    iter_top(cols_,i);

    for (; i.ok(); i++) {
	assert(i->when() != w);
	if (i->when() > w)
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
	if (i->when() == w && i->musical_ == mus)
	    return i;
	if (i->when() > w)
	    break;
    }
    i = create_cols(w);
    if (mus)
	i++;
    return i;
}

void
Score::do_cols()
{
    iter_top(cols_,i);
    for (; i.ok(); i++) {
	pscore_p_->add(i->pcol_l_);
    }
    clean_cols();    // can't move clean_cols() farther up.
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
	assert(i->score_l_ == this);
    }
    staffs_.OK();
    cols_.OK();
    for (iter_top(cols_,cc); cc.ok() && (cc+1).ok(); cc++) {
	assert(cc->when() <= (cc+1)->when());
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
    if (pscore_p_)
	pscore_p_->print();
    
    mtor << "}\n";
#endif
}

Score::Score(Paperdef*p)
{
    pscore_p_=0;
    paper_p_ = p;		// ?? safe?
}

Score::~Score()
{
    delete pscore_p_;
    delete paper_p_;
}

void
Score::output(String s)
{
    OK();
    if (paper_p_->outfile=="")
	paper_p_->outfile = s;
    
    *mlog << "output to " << paper_p_->outfile << "...\n";
    Tex_stream the_output(paper_p_->outfile);    
    pscore_p_->output(the_output);
}



void
Score::add(Staff*s)
{
    s->score_l_ = this;
    staffs_.bottom().add(s);
}

