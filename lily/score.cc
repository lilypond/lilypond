/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "tex-stream.hh"
#include "score.hh"
#include "score-column.hh"
#include "p-score.hh"
#include "staff.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "main.hh"
#include "source.hh"
#include "source-file.hh"
#include "score-walker.hh"
#include "midi-output.hh"
#include "midi-def.hh"
#include "pulk-voices.hh"
#include "request-column.hh"
#include "p-col.hh"

extern String default_out_fn;

void
Score::setup_music()
{
    *mlog << "\nSetting up requests..." << flush;
    
    Pulk_voices pulk(staffs_); 

    Moment l_mom = pulk.last_;
    if (l_mom == Moment(0)) {
	errorlevel_i_ |= 1;
	input_.error("Need to have music in a score.");
    }
    
    Moment previous_mom = -1;
    while (pulk.ok()) {

	Moment w= pulk.next_mom();
	assert(w > previous_mom);
	Request_column* rcol_p = new Request_column( staffs_ );

	Score_column* c1 = new Score_column(w);
	Score_column* c2 = new Score_column(w);
	if (w == Moment(0) || w == l_mom) {
	    c1->set_breakable();
	}
		
	c1->musical_b_ = false;
	c2->musical_b_ = true;
	
	cols_.bottom().add(c1);
	cols_.bottom().add(c2);
	rcol_p->set_score_cols(c1, c2);
	rcols_.bottom().add(rcol_p);
	pulk.get_aligned_request( rcol_p );
	previous_mom =w;
    }

    errorlevel_i_ |= pulk.time_checks_failed_b(); 
}

void
Score::process_music()
{
    *mlog << "Processing requests ..." << flush;
    for (Score_walker w(this); w.ok(); w++) {
	w.process();
   }
}

void
Score::process()
{
    setup_music();

    paper();
    midi();
}

void
Score::paper()
{
    if (!paper_p_)
	return;
    if( errorlevel_i_){
	// should we? hampers debugging. 
	warning("Errors found, /*not processing score*/");
//	return;
    }
    pscore_p_ = new PScore(paper_p_);
    do_cols();
    
    for (iter_top(staffs_,i); i.ok(); i++) 
	i->set_output(pscore_p_);

    
    process_music();
    clean_cols();    // can't move clean_cols() farther up.
    print();
    calc_idealspacing();

    // debugging
    OK();
    *mlog << endl;
    pscore_p_->process();

    // output
    paper_output();
    
}

/**
  Remove empty cols, preprocess other columns.
  */
void
Score::clean_cols()
{
#if 1
    for (iter_top(staffs_,i); i.ok(); i++)
	i->clean_cols();

    for (iter_top(rcols_,i); i.ok(); i++) {
	i->when();		// update cache, ugh
	if (!i->command_column_l_->used_b()) {
	    i->command_column_l_ = 0;
	}
	if (!i->musical_column_l_->used_b())
	    i->musical_column_l_ = 0;
    }
    
    for (iter_top(cols_,c); c.ok(); ) {
	if (!c->pcol_l_->used_b()) {
	    delete c.remove_p();
	} else {
	    c->preprocess();
	    c++;
	}
    }
#endif
}

PCursor<Score_column*>
Score::find_col(Moment w, bool mus)
{
    iter_top( cols_,i);
    
    for (; i.ok(); i++) {
	if (i->when() == w && i->musical_b_ == mus)
	    return i;
	if (i->when() > w)
	    break;
    }
    assert(false);
    return i;
}

void
Score::do_cols()    
{
    iter_top(cols_,i);
    for (; i.ok(); i++) {
	pscore_p_->add(i->pcol_l_);
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
Score::set(Paper_def *pap_p)
{
    delete paper_p_;
    paper_p_ = pap_p;
}

void
Score::set(Midi_def* midi_p)
{    
    delete midi_p_;
    midi_p_ = midi_p;
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
    if (midi_p_)
	midi_p_->print();
    
    mtor << "}\n";
#endif
}

Score::Score()
{
    pscore_p_=0;
    paper_p_ = 0;
    midi_p_ = 0;
    errorlevel_i_ = 0;
}

Score::~Score()
{
    delete pscore_p_;
    delete paper_p_;
    delete midi_p_;
}

void
Score::paper_output()
{
    if (paper_p_->outfile=="")
	paper_p_->outfile = default_out_fn + ".out";

    if ( errorlevel_i_ ) { 
	*mlog << "lilypond: warning: no output to: " << paper_p_->outfile 
	<< " (errorlevel=" << errorlevel_i_ << ")" << endl;
        return;
    }

    *mlog << "TeX output to " << paper_p_->outfile << " ...\n";
    
    Tex_stream the_output(paper_p_->outfile);
    
    the_output << "% outputting Score, defined at: " <<
	input_.location_str() << "\n";
    pscore_p_->output(the_output);
    
}

void
Score::midi()
{
    if (!midi_p_)
	return;

    if (midi_p_->outfile_str_ == "")
	midi_p_->outfile_str_ = default_out_fn + ".midi";
    
    *mlog << "midi output to " << midi_p_->outfile_str_ << " ...\n";    
    Midi_output(this, midi_p_);
}

void
Score::add(Staff*s)
{
    s->score_l_ = this;
    staffs_.bottom().add(s);
}
