/*
  staff-performer.cc -- implement Staff_performer

  (c) 1996, 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */


#include "staff-performer.hh"
#include "translator.hh"
#include "input-translator.hh"
#include "debug.hh"
#include "midi-def.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "string.hh"
#include "string-convert.hh"

IMPLEMENT_STATIC_NAME(Staff_performer);
IMPLEMENT_IS_TYPE_B1(Staff_performer,Performer_group_performer);
ADD_THIS_PERFORMER(Staff_performer);

Staff_performer::Staff_performer()
{
    midi_mom_ = 0;
    track_i_ = 0;
    midi_track_p_ = 0;
}

Staff_performer::~Staff_performer()
{
     delete midi_track_p_;
}

void
Staff_performer::header()
{
    // set track name
    Midi_text track_name( Midi_text::TRACK_NAME, "Track " + String_convert::i2dec_str( track_i_, 0, '0' ) );
    midi_track_p_->add( Moment( 0 ), &track_name );

    // set instrument :-)
    Midi_text instrument_name( Midi_text::INSTRUMENT_NAME, instrument_str() );
    midi_track_p_->add( Moment( 0 ), &instrument_name );

    Midi_tempo midi_tempo( midi_l_->get_tempo_i( Moment( 1, 4 ) ) );
    midi_track_p_->add( Moment( 0 ), &midi_tempo );
}

String 
Staff_performer::instrument_str() 
{ 
    return Translator::id_str_; 
}

void
Staff_performer::midi_output( Midi_stream* midi_stream_l )
{
    *mlog << "[" << track_i_ << "]";
    *midi_stream_l << *midi_track_p_;
}

void 
Staff_performer::play_event( Midi_item* l )
{
    Moment mom = get_mom();
    Moment delta_t = mom - midi_mom_ ;
    midi_mom_ += delta_t;
    midi_track_p_->add( delta_t, l );    
}

void
Staff_performer::set_track( Midi_def* midi_l, int& track_i_r ) 
{ 
    midi_l_ = midi_l;
    track_i_ = track_i_r++;
    midi_track_p_ = new Midi_track( track_i_ );
    header();
}

