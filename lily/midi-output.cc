/*
  midioutput.cc -- implement Midi_output

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>, Jan Nieuwenhuizen <jan@digicash.com> 
*/

// "" huh?
#include "time.h"

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
#include "musicalrequest.hh"


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
    midi_track.add( Moment( 0.0 ), &track_name );

    // set instrument :-)
    Midi_text instrument_name( Midi_text::INSTRUMENT_NAME, "piano" );
    midi_track.add( Moment( 0.0 ), &instrument_name );

    // set key, help, where to get key, where to get major/minor?
    int accidentals_i = 0;
    int minor_i = 0;

    // uph, sorry, wanna test this...
    // menuetto in F
    if ( ( infile_str_g.index_i( "scsii-menuetto" ) >= 0 )
	|| ( infile_str_g.index_i( "standchen" ) >= 0 )
	|| ( infile_str_g.left_str( 1 )  == String( "s" ) ) )
    	accidentals_i = -1;
    // standchen in d	
    if ( ( infile_str_g.index_i( "standchen" ) >= 0 ) )
    	minor_i = 1;

    Midi_key midi_key( accidentals_i, minor_i ); 
    midi_track.add( Moment( 0.0 ), &midi_key );

    Midi_tempo midi_tempo( midi_l_->get_tempo_i( Moment( 1, 4 ) ) );
    midi_track.add( Moment( 0.0 ), &midi_tempo );

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
    String str = String( "Creator: " ) + get_version() + "\n";
    str += "Generated, at ";
    str += ctime( &t );
    str += ", from musical definition: " + infile_str_g;
    str += "\n";    

    Midi_text creator( Midi_text::TEXT, str );
    midi_track.add( Moment( 0.0 ), &creator );

    struct tm* tm_l = gmtime( &t );
    String year_str = String_convert::i2dec_str( 1900 + tm_l->tm_year, 4, '0' );
	    
    // your copyleft here
    str = " Copyleft (o) " + year_str;
    str += " Han-Wen Nienhuys <hanwen@stack.nl>, "
	" Jan Nieuwenhuizen <jan@digicash.com>\n";
	
    Midi_text copyleft( Midi_text::COPYRIGHT, str );
    midi_track.add( Moment( 0.0 ), &copyleft );
    *midi_stream_l_  << midi_track;
}

void
Midi_output::staffs()
{
    int track_i = 1;
    for (iter_top(score_l_->staffs_,i); i.ok(); i++)
	do_staff(i, track_i++);
}

