//
// my-midi-parser.cc -- implement My_midi_parser
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "string-convert.hh"
#include "duration-convert.hh"
#include "mi2mu-global.hh"
#include "my-midi-lexer.hh"
#include "my-midi-parser.hh"
#include "mudela-column.hh"
#include "mudela-item.hh"
#include "mudela-score.hh"
#include "mudela-staff.hh"

void
yyerror(char const* sz_l)
{
    midi_parser_l_g->error (sz_l);
}


My_midi_parser* midi_parser_l_g = 0;

My_midi_parser::My_midi_parser (String filename_str, Sources *sources_l)
{
    filename_str_ = filename_str;
    midi_lexer_p_ = new My_midi_lexer (filename_str_,sources_l);
    midi_lexer_l_g = midi_lexer_p_;    // ugh

    bar_i_ = 1;

    defined_ch_C_ = 0;
    fatal_error_i_ = 0;
    
    mudela_column_l_ = 0;
    mudela_score_p_ = new Mudela_score (1, 1, 1);


    // ugh, belong to Mudela_{score,staff}
    track_i_ = 0;
    mudela_staff_l_ = 0;
    mudela_key_p_ = 0;
    mudela_tempo_p_ = 0;
    mudela_meter_p_ = 0;

    reset();
}

My_midi_parser::~My_midi_parser()
{
    midi_lexer_l_g = 0;    // ugh

    delete midi_lexer_p_;
    delete mudela_key_p_;
    delete mudela_tempo_p_;
    delete mudela_meter_p_;
    delete mudela_score_p_;
}

void
My_midi_parser::reset()
{
//    open_mudela_note_l_list_.clear();
    open_mudela_note_l_list_.junk_links();

    // ugh
    delete mudela_key_p_;
    mudela_key_p_ = new Mudela_key (0, 0);
    // useconds per 4: 250000 === 60 4 per minute
    delete mudela_tempo_p_;
    mudela_tempo_p_ = new Mudela_tempo (1000000);
    delete mudela_meter_p_;
    mudela_meter_p_ = new Mudela_meter (4, 2, 24, 8);

    bar_i_ = 1;
    mudela_column_l_ = mudela_score_p_->mudela_column_l (0);

    // ugh
    copyright_str_ = "";
    track_name_str_ = "";
    instrument_str_ = "";
}

void
My_midi_parser::add_score (Mudela_score* mudela_score_p)
{
    assert (mudela_score_p_);

#if 0 // ugh, already constructed
    mudela_score_p_ = mudela_score_p;
    if  (!mudela_column_l_)
	mudela_column_l_ = mudela_score_p_->mudela_column_l (0);
#endif
	
    mudela_score_p_->mudela_key_l_ = mudela_key_p_;
    mudela_score_p_->mudela_meter_l_ = mudela_meter_p_;
    mudela_score_p_->mudela_tempo_l_ = mudela_tempo_p_;
    bar_i_ = 1;
}

void
My_midi_parser::error (char const* sz_l)
{
    midi_lexer_l_g->error (sz_l);

    if  (fatal_error_i_)
	exit (fatal_error_i_);
}

void
My_midi_parser::forward (int i)
{
    if  (!i)
	return;

    Duration dur;
    dur.type_i_ = (0);
    dur.set_ticks (i);
    Moment mom = at_mom() + Duration_convert::dur2_mom (dur);

    mudela_column_l_ = mudela_score_p_->mudela_column_l (mom);

    if  (i) {
	int bars_i = (int) (mom / mudela_meter_p_->bar_mom());
	if  (bars_i > bar_i_)
	    LOGOUT(NORMAL_ver) << '[' << bar_i_ << ']' << flush; 
	bar_i_ = bars_i;
    }
}

Moment
My_midi_parser::at_mom()
{
    assert (mudela_column_l_);
//    return mudela_column_l_ ? mudela_column_l_->at_mom() : 0;
    return mudela_column_l_->at_mom();
}

void
My_midi_parser::note_begin (int channel_i, int pitch_i, int dyn_i)
{
    // junk dynamics
    (void)dyn_i;

    Mudela_note* p = new Mudela_note (mudela_column_l_, channel_i, pitch_i, dyn_i);
//  ugh, score doesn't know about last staff yet...
//    mudela_score_p_->add_item (p);
    mudela_staff_l_->add_item (p);
    open_mudela_note_l_list_.bottom().add (p);
}

void
My_midi_parser::note_end (int channel_i, int pitch_i, int aftertouch_i)
{
    // junk dynamics
    (void)aftertouch_i;

    // find 
    for  (PCursor<Mudela_note*> i (open_mudela_note_l_list_); i.ok(); i++) {
	if  ( (i->pitch_i_ == pitch_i) &&  (i->channel_i_ == channel_i)) {
	    i->end_column_l_ = mudela_column_l_;
	    LOGOUT(DEBUG_ver) << "Note: " << pitch_i;
	    LOGOUT(DEBUG_ver) << "; " << i->mudela_column_l_->at_mom_;
	    LOGOUT(DEBUG_ver) << ", " << i->end_column_l_->at_mom_ << "\n";
	    i.remove_p();
	    return;
	}
    }
    warning (String ("junking note-end event: ")
	+ " channel = " + String_convert::i2dec_str (channel_i, 0, ' ')
	+ ", pitch = " + String_convert::i2dec_str (pitch_i, 0, ' '));
}

void
My_midi_parser::note_end_all()
{
    // find 
    for  (PCursor<Mudela_note*> i (open_mudela_note_l_list_); i.ok(); i++) {
	i->end_column_l_ = mudela_column_l_;
	i.remove_p();
	// ugh
	if  (!i.ok())
	    break;
    }
}

int
My_midi_parser::parse()
{
    LOGOUT(NORMAL_ver) << "\nParsing..." << flush;
    int i = ::yyparse();
    if  (!i)
	note_end_all();
    return i;
}

void
My_midi_parser::set_division_4 (int division_4_i)
{
    division_1_i_ = division_4_i * 4;
    // ugh
    Duration::division_1_i_s = division_1_i_;
    if  (division_4_i < 0)
	warning ("seconds iso metrical time");
}

void
My_midi_parser::set_key (int accidentals_i, int minor_i)
{
    delete mudela_key_p_;
    mudela_key_p_ = new Mudela_key (accidentals_i, minor_i);
}

void
My_midi_parser::set_meter (int num_i, int den_i, int clocks_i, int count_32_i)
{
    delete mudela_meter_p_;
    mudela_meter_p_ = new Mudela_meter (num_i, den_i, clocks_i, count_32_i);
}

void
My_midi_parser::set_tempo (int useconds_per_4_i)
{
    delete mudela_tempo_p_;
    mudela_tempo_p_ = new Mudela_tempo (useconds_per_4_i);
}

