/*
  score-performer.cc -- implement Score_performer

  (c) 1996, 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */

#include <time.h>
#include "score-performer.hh"
#include "input-translator.hh"
#include "midi-def.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "main.hh"
#include "score.hh"
#include "source-file.hh"
#include "source.hh"

IMPLEMENT_IS_TYPE_B1(Score_performer,Performer_group_performer);
ADD_THIS_PERFORMER(Score_performer);

Score_performer::Score_performer()
{
    midi_l_ = 0;
}

Score_performer::~Score_performer()
{
}

Translator* 
Score_performer::ancestor_l( int l ) 
{ 
    return Global_translator::ancestor_l( l );
}

int 
Score_performer::depth_i() const 
{ 
    return Global_translator::depth_i();
}

void
Score_performer::finish()
{
    Performer_group_performer::do_removal_processing();


    Midi_stream output_stream( midi_l_->outfile_str_, midi_item_p_arr_.size() + 1, 384 );    
    *mlog << "MIDI output to " << midi_l_->outfile_str_ << " ..." << endl;    

    header( output_stream);
    int track_i = 1;
    for (int i=0; i<  midi_item_p_arr_.size(); i++) {
	Midi_item * it_p = midi_item_p_arr_[i];
	
	if ( it_p->is_type_b( Midi_track::static_name()))
	    ((Midi_track*)it_p )->number_i_ = track_i ++;
	output_stream<< *it_p;
    }
    *output_stream.os_p_ << flush;
    *mlog << endl;
}

void
Score_performer::play_event(Midi_item*m)
{
    midi_item_p_arr_.push(m);
}

Moment
Score_performer::get_mom() const
{
    return now_mom_;
}

void
Score_performer::header(Midi_stream &output_stream)
{
    Midi_track midi_track;
    
    time_t t = time( 0 );

    // perhaps multiple text events?
    String str = String( "Creator: " ) + get_version_str() + "\n";

    Midi_text creator( Midi_text::TEXT, str );
    midi_track.add( Moment( 0 ), &creator );

    str = "Automatically generated at ";
    str += ctime( &t );
    str = str.left_str( str.length_i() - 1 );
    str += "\n";
    Midi_text generate( Midi_text::TEXT, str );
    midi_track.add( Moment( 0 ), &generate );

    str = "from musical definition: ";

    str += score_l_->location_str();
    Midi_text from( Midi_text::TEXT, str );
    midi_track.add( Moment( 0 ), &from );

    Midi_text track_name( Midi_text::TRACK_NAME, "Track " 
			  + String_convert::i2dec_str( 0, 0, '0' ) );
    midi_track.add( Moment( 0 ), &track_name );

    output_stream  << midi_track;
}

void 
Score_performer::prepare( Moment m )
{
    now_mom_ = m;
}

void 
Score_performer::process()
{
    process_requests();
    prev_mom_ = now_mom_;
}

void
Score_performer::set_score(Score* score_l )
{
    Global_translator::set_score( score_l );
    midi_l_ = score_l->midi_p_;
}

void
Score_performer::start()
{
    if ( midi_l_->outfile_str_ == "" )
	midi_l_->outfile_str_ = default_out_fn + ".midi";
}


int
Score_performer::get_tempo_i()const
{
    return midi_l_->get_tempo_i(Moment( 1, 4 ));
}
