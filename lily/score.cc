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
#include "music-iterator.hh"
#include "music.hh"
#include "global-translator.hh"

extern String default_out_fn;

Score::Score(Score const &s)
{
    assert(!pscore_p_);
    music_p_ = s.music_p_->clone();
    midi_p_ = new Midi_def(*s.midi_p_);
    paper_p_ = new Paper_def(*s.paper_p_);
}

void
Score::run_translator(Global_translator * trans_l)
{
    trans_l->set_score (this);
    Music_iterator * iter = Music_iterator::static_get_iterator_p(music_p_, 
								  trans_l);
    iter->construct_children();

    while ( iter->ok() || trans_l->moments_left_i() ) {
	Moment w = INFTY;
	if (iter->ok() ) {
	    w = iter->next_moment();
	    iter->print();
	}
	trans_l->modify_next( w );
	trans_l->prepare(w);
	iter->process_and_next( w );
	trans_l->process();
    }
    delete iter;
    trans_l->finish();
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
    
    *mlog << "\nCreating elements ..." << flush;
    pscore_p_ = new PScore(paper_p_);
    
    Global_translator * score_trans=  paper_p_->get_global_translator_p();
    run_translator( score_trans );
    delete score_trans;
    
    if( errorlevel_i_){
	// should we? hampers debugging. 
	warning("Errors found, /*not processing score*/");
//	return;
    }
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
    for (iter_top(cols_,c); c.ok(); ) {
	if (!c->pcol_l_->used_b()) {
	    delete c.remove_p();
	} else {
	    c->preprocess();
	    c++;
	}
    }
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
    if (paper_p_->outfile_str_=="")
	paper_p_->outfile_str_ = default_out_fn + ".out";

    if ( errorlevel_i_ ) { 
	*mlog << "lilypond: warning: no output to: " << paper_p_->outfile_str_ 
	<< " (errorlevel=" << errorlevel_i_ << ")" << endl;
        return;
    }

    *mlog << "TeX output to " << paper_p_->outfile_str_ << " ...\n";
    
    Tex_stream the_output(paper_p_->outfile_str_);
    
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

