/*
  score.cc -- implement Score

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "tstream.hh"
#include "score.hh"
#include "sccol.hh"
#include "pscore.hh"
#include "staff.hh"
#include "debug.hh"
#include "paperdef.hh"
#include "main.hh"
#include "source.hh"
#include "sourcefile.hh"
#include "scorewalker.hh"
#include "mididef.hh"
#include "midiitem.hh"
#include "midistream.hh"
#include "midicolumn.hh"
#include "midistaff.hh"
#include "midiwalker.hh"

void
Score::setup_music()
{
    *mlog << "\nSetting up music ..." << flush;
    if (last() == Moment(0)) {
	errorlevel_i_ |= 1;
	error("Need to have music in a score.", defined_ch_c_l_);
    }

    pscore_p_ = new PScore(paper_p_);
    find_col(0, false)->set_breakable(); // ugh
    find_col(last(), false)->set_breakable();
    
	

    for (iter_top(staffs_,i); i.ok(); i++) {
	i->set_output(pscore_p_);
	i->setup_staffcols();
	i->OK();
    }
}

void
Score::process_music()
{
    *mlog << "Processing music ..." << flush;
    for (Score_walker w(this); w.ok(); w++) {
	w.process();
   }
}

void
Score::process()
{
    setup_music();

    process_music();

    // do this after processing, staffs first have to generate PCols.
    do_cols();
    print();
    calc_idealspacing();

    // debugging
    OK();
    *mlog << endl;
    pscore_p_->process();
}

/**
  Remove empty cols, preprocess other columns.
  */
void
Score::clean_cols()
{
    for (iter_top(staffs_,i); i.ok(); i++)
	i->clean_cols();

    for (iter_top(cols_,c); c.ok(); ) {
	if (!c->pcol_l_->used()) {
	    delete c.get();
	} else {
	    c->preprocess();
	    c++;
	}
    }
}

/**
  Create columns at time #w#.
  this sux.  We should have Score_column create the appropriate PCol.
  Unfortunately, PCols don't know about their position.

  @return cursor pointing to the nonmusical (first) column
  */
PCursor<Score_column*>
Score::create_cols(Moment w)
{
    Score_column* c1 = new Score_column(w);
    Score_column* c2 = new Score_column(w);
    
    c1->musical_b_ = false;
    c2->musical_b_ = true;
    
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
Score::find_col(Moment w, bool mus)
{
    iter_top( cols_,i);
    
    for (; i.ok(); i++) {
	if (i->when() == w && i->musical_b_ == mus)
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
Score::set(Mididef* midi_p)
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
    
    mtor << "}\n";
#endif
}

Score::Score(Paperdef*paper_p)
{
    pscore_p_=0;
    paper_p_ = paper_p;
    midi_p_ = 0;
    errorlevel_i_ = 0;
    defined_ch_c_l_ = 0;
}

Score::~Score()
{
    delete pscore_p_;
    delete paper_p_;
    delete midi_p_;
}

void
Score::output(String s)
{
    OK();
    if (paper_p_->outfile=="")
	paper_p_->outfile = s;
    
    if ( errorlevel_i_ ) { 
	*mlog << "lilypond: warning: no output to: " << paper_p_->outfile 
	<< " (errorlevel=" << errorlevel_i_ << ")" << endl;
        return;
    }

    *mlog << "output to " << paper_p_->outfile << "...\n";
    
    Tex_stream the_output(paper_p_->outfile);
    
    the_output << "% outputting Score, defined at: " <<
	source_global_l->
	sourcefile_l (defined_ch_c_l_)->file_line_no_str(defined_ch_c_l_) << "\n";
    pscore_p_->output(the_output);
}

void
Score::midi()
{
    if (!midi_p_)
	return;

    *mlog << "midi output to " << midi_p_->outfile_str_ << "...\n";

    int track_i = 0;
    for ( PCursor<Staff*> staff_l_pcur( staffs_.top() ); staff_l_pcur.ok(); staff_l_pcur++ ) {
	Midi_staff* mstaff_l = (Midi_staff*)*staff_l_pcur;
	if ( !mstaff_l->pscore_l_ ) // we _are_ a midi-staff, ugh
	    track_i++;
    }

    Midi_stream midi_stream( midi_p_->outfile_str_, track_i, midi_p_->get_tempo_i( Moment( 1, 4 ) )  );

    track_i = 0;
    for ( PCursor<Staff*> staff_l_pcur( staffs_.top() ); staff_l_pcur.ok(); staff_l_pcur++ ) {
	Midi_staff* mstaff_l = (Midi_staff*)*staff_l_pcur;
	if ( !mstaff_l->pscore_l_ ) // we _are_ a midi-staff, ugh
	    mstaff_l->midi( &midi_stream, track_i++ );
    }
}

void
Score::add(Staff*s)
{
    s->score_l_ = this;
    staffs_.bottom().add(s);
}
