/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "tex-stream.hh"
#include "score.hh"
#include "score-column.hh"
#include "p-score.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "main.hh"
#include "source.hh"
#include "source-file.hh"
#include "midi-output.hh"
#include "midi-def.hh"
#include "p-col.hh"
#include "score-reg.hh"
#include "music-iterator.hh"
#include "music.hh"
#include "music-list.hh"
#include "input-register.hh"

extern String default_out_fn;

Score::Score(Score const &s)
{
    assert(!pscore_p_);
    music_p_ = s.music_p_->clone();
    midi_p_ = new Midi_def(*s.midi_p_);
    paper_p_ = new Paper_def(*s.paper_p_);
}

void
Score::setup_music()
{
    *mlog << "\nSetting up requests..." << flush;
    
    Score_register * score_reg =  
	(Score_register*)lookup_reg("Score_register")->get_group_register_p();

    score_reg->set_score (this);
    Music_iterator * iter = Music_iterator::static_get_iterator_p(music_p_, 
								  score_reg);
    iter->construct_children();

    while ( iter->ok() || score_reg->extra_mom_pq_.size() ) {
	Moment w = INFTY;
	if (iter->ok() ) 
	    w = iter->next_moment();
	
	if (score_reg->extra_mom_pq_.size() && 
	    score_reg->extra_mom_pq_.front() <= w)
	    
	    w = score_reg->extra_mom_pq_.get();

	mtor << "processing moment " << w << "\n";

	Score_column* c1 = new Score_column(w);
	Score_column* c2 = new Score_column(w);
	
	c1->musical_b_ = false;
	c2->musical_b_ = true;
	
	cols_.bottom().add(c1);
	cols_.bottom().add(c2);
	score_reg->set_cols(c1,c2);
	
	score_reg->post_move_processing();
	iter->next( w );
	
	score_reg->process_requests();
	score_reg->do_announces();
	score_reg->pre_move_processing();
    }
    delete iter;
    score_reg->do_removal_processing();
    delete score_reg;
}

void
Score::process()
{
    paper();
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
    setup_music();
    do_cols();
    
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
    // TODO
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
    music_p_->print();
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
    delete music_p_;
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
	location_str() << "\n";
    pscore_p_->output(the_output);
    
}

void
Score::midi()
{
#if 0
    if (!midi_p_)
	return;

    if (midi_p_->outfile_str_ == "")
	midi_p_->outfile_str_ = default_out_fn + ".midi";
    
    *mlog << "midi output to " << midi_p_->outfile_str_ << " ...\n";    
    Midi_output(this, midi_p_);
#endif
}

