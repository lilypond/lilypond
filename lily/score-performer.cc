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
IMPLEMENT_STATIC_NAME(Score_performer);
ADD_THIS_PERFORMER(Score_performer);

Score_performer::Score_performer()
{
    midi_stream_p_ = 0;
    midi_l_ = 0;
}

Score_performer::~Score_performer()
{
    delete midi_stream_p_;
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
    *mlog << "MIDI output to " << midi_l_->outfile_str_ << " ..." << endl;    
    *mlog << "tracks: ";
    header();
    Performer_group_performer::midi_output( midi_stream_p_ );
    *mlog << endl;
}

Moment
Score_performer::get_mom() const
{
    return now_mom_;
}

void
Score_performer::header()
{
    int track_i = 0;
    Midi_track midi_track( track_i );
    
    time_t t = time( 0 );

    // perhaps multiple text events?
    String str = String( "Creator: " ) + get_version_str() + "\n";

    Midi_text creator( Midi_text::TEXT, str );
    midi_track.add( Moment( 0 ), &creator );

    str = "Generated, at ";
    str += ctime( &t );
    str = str.left_str( str.length_i() - 1 );
    str += ",\n";
    Midi_text generate( Midi_text::TEXT, str );
    midi_track.add( Moment( 0 ), &generate );

    str = "from musical definition: ";
#if 0    
    Source_file* sourcefile_l = source_l_g->sourcefile_l( score_l_->defined_ch_C_ );
    if ( sourcefile_l )
	str += sourcefile_l->name_str();
#elif 1
    str += score_l_->location_str();
#else
    str += score_l_->
#endif
    Midi_text from( Midi_text::TEXT, str );
    midi_track.add( Moment( 0 ), &from );

    // set track name
    Midi_text track_name( Midi_text::TRACK_NAME, "Track " + String_convert::i2dec_str( 0, 0, '0' ) );
    midi_track.add( Moment( 0 ), &track_name );

    // ugh, to please lily when reparsing mi2mu output.
    // lily currently barfs when no meter present.
    /* are you sure? init is to 4/4 HWN */
    Midi_time midi_time( 4, 4, 18 );
    midi_track.add( Moment( 0.0 ), &midi_time );

    *mlog << "[" << track_i << "]";
    *midi_stream_p_  << midi_track;
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
Score_performer::set_score( Score* score_l )
{
    /*
     why's there no start() when there's a finish()?
     let's misuse this for start()
     */
    Global_translator::set_score( score_l );
    midi_l_ = score_l->midi_p_;
}

void
Score_performer::start()
{
    int track_i = 1;
    Performer_group_performer::set_track( midi_l_, track_i );

    if ( midi_l_->outfile_str_ == "" )
	midi_l_->outfile_str_ = default_out_fn + ".midi";
    
    midi_stream_p_ = new Midi_stream( midi_l_->outfile_str_, track_i, 384 );
}

