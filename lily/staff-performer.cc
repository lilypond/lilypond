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


IMPLEMENT_IS_TYPE_B1(Staff_performer,Performer_group_performer);
ADD_THIS_PERFORMER(Staff_performer);

Staff_performer::Staff_performer()
{
    midi_mom_ = 0;
    midi_track_p_ = new Midi_track;
}
void
Staff_performer::do_creation_processing()
{
    header();
}

void
Staff_performer::do_removal_processing()
{
    Performer::play_event( midi_track_p_);
}

Staff_performer::~Staff_performer()
{
    delete midi_track_p_;
}

void
Staff_performer::header()
{
    // set track name
    Midi_text track_name( Midi_text::TRACK_NAME, instrument_str());
    midi_track_p_->add( Moment( 0 ), &track_name );

    // set instrument description
    Midi_text instrument_name( Midi_text::INSTRUMENT_NAME, instrument_str() );
    midi_track_p_->add( Moment( 0 ), &instrument_name );

    // set instrument :-)
    // lieve wendy, nu heb je mijn track_i_ / get_staff_i weggehaald...
    // zie ook note-performer: ugh
    // ugh, need to know channel (===track===staff) too
    int channel_i = 0;
    Midi_instrument instrument( channel_i, instrument_str() );
    midi_track_p_->add( Moment( 0 ), &instrument );

    Midi_tempo midi_tempo( get_tempo_i(  ) );
    midi_track_p_->add( Moment( 0 ), &midi_tempo );
}

String 
Staff_performer::instrument_str() 
{ 
    return Translator::id_str_; 
}

void 
Staff_performer::play_event( Midi_item* l )
{
    Moment mom = get_mom();
    Moment delta_t = mom - midi_mom_ ;
    midi_mom_ += delta_t;
    midi_track_p_->add( delta_t, l);
}

