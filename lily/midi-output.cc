/*
  midioutput.cc -- implement Midi_output

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>, Jan Nieuwenhuizen <jan@digicash.com> 
*/

// "" huh?
#include "time.h"
#include "main.hh"
#include "source.hh"
#include "proto.hh"
#include "plist.hh"
#include "string.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "score.hh"
#include "staff.hh"
#include "main.hh"
#include "midi-stream.hh"
#include "midi-def.hh"
#include "midi-output.hh"
#include "midi-walker.hh"
#include "midi-item.hh"
#include "staff-column.hh"
#include "musical-request.hh"


Midi_output::Midi_output(Score* score_l, Midi_def* midi_l )
{
    midi_l_ = midi_l;
    score_l_ = score_l;

    Midi_stream midi_stream(midi_l->outfile_str_, 
        // don-t forget: extra track 0 for tempo/copyright stuff...
    	score_l_->staffs_.size() + 1, 
	384 );
    midi_stream_l_ = &midi_stream;

    header();
    staffs();
}

void
Midi_output::do_staff(Staff*st_l,int track_i)
{
    Midi_track midi_track( track_i );

    // set track name
    Midi_text track_name( Midi_text::TRACK_NAME, "Track " + String_convert::i2dec_str( track_i, 0, '0' ) );
    midi_track.add( Moment( 0 ), &track_name );

    // set instrument :-)
    Midi_text instrument_name( Midi_text::INSTRUMENT_NAME, "piano" );
    midi_track.add( Moment( 0 ), &instrument_name );

    Midi_tempo midi_tempo( midi_l_->get_tempo_i( Moment( 1, 4 ) ) );
    midi_track.add( Moment( 0 ), &midi_tempo );

    for (Midi_walker w (st_l, &midi_track); w.ok(); w++)
	w.process_requests();

    *midi_stream_l_  << midi_track;
}  

void
Midi_output::header()
{
    Midi_track midi_track( 0 );
    
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
    Source_file* sourcefile_l = source_l_g->sourcefile_l( score_l_->defined_ch_C_ );
    if ( sourcefile_l ) {
	str = "from musical definition: " 
	    + sourcefile_l->file_line_no_str(score_l_->defined_ch_C_);

    }
    Midi_text from( Midi_text::TEXT, str );
    midi_track.add( Moment( 0 ), &from );

    // set track name
    Midi_text track_name( Midi_text::TRACK_NAME, "Track " + String_convert::i2dec_str( 0, 0, '0' ) );
    midi_track.add( Moment( 0 ), &track_name );

    // ugh, to please lily when reparsing mi2mu output.
    // lily currently barfs when no meter present.
    Midi_time midi_time( 4, 4, 18 );
    midi_track.add( Moment( 0.0 ), &midi_time );

    *midi_stream_l_  << midi_track;
}

void
Midi_output::staffs()
{
    int track_i = 1;
    for (iter_top(score_l_->staffs_,i); i.ok(); i++)
	do_staff(i, track_i++);
}

