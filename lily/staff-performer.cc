/*
  staff-performer.cc -- implement Staff_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
 */


#include "staff-performer.hh"
#include "translator.hh"
#include "input-translator.hh"
#include "debug.hh"
#include "midi-def.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "string.hh"
#include "string-convert.hh"


IMPLEMENT_IS_TYPE_B1(Staff_performer,Performer_group_performer);
ADD_THIS_PERFORMER(Staff_performer);

Staff_performer::Staff_performer()
{
    midi_track_p_ = new Midi_track;
}

Staff_performer::~Staff_performer()
{
    delete midi_track_p_;
}

void
Staff_performer::do_creation_processing()
{
}

void
Staff_performer::do_removal_processing()
{
    header();

    Moment midi_mom = 0;
    for ( PCursor<Audio_item*> i( audio_item_p_list_ ); i.ok(); i++ ) {
	Audio_item* l = *i;
	Moment mom = l->audio_column_l_->at_mom();
	Moment delta_t = mom - midi_mom_ ;
	midi_mom_ += delta_t;
	Midi_item* p = l->midi_item_p();
	p->channel_i_ = track_i_;
	midi_track_p_->add( delta_t, p );
	delete p;
    }

    Performer::play( midi_track_p_ );
}

void
Staff_performer::header()
{
    // set track name
    Midi_text track_name( Midi_text::TRACK_NAME, instrument_str() );
    midi_track_p_->add( Moment( 0 ), &track_name );

    // set instrument description
    Midi_text instrument_name( Midi_text::INSTRUMENT_NAME, instrument_str() );
    midi_track_p_->add( Moment( 0 ), &instrument_name );

    // set instrument :-)
    // lieve wendy, nu heb je mijn track_i_ / get_staff_i weggehaald...
    // zie ook note-performer: ugh
    // ugh, need to know channel (===track===staff) too
    int channel_i = track_i_;
    Midi_instrument instrument( channel_i, instrument_str() );
    midi_track_p_->add( Moment( 0 ), &instrument );

    Midi_tempo midi_tempo( get_tempo_i() );
    midi_track_p_->add( Moment( 0 ), &midi_tempo );
}

String 
Staff_performer::instrument_str() 
{ 
    return Translator::id_str_; 
}

void 
Staff_performer::play( Audio_item* p )
{
    audio_item_p_list_.bottom().add( p );
    Performer::play( p );
}

// huh?
void 
Staff_performer::play( Midi_item* p )
{
    Performer::play( p );
}


//<ugh>
int
Staff_performer::get_track_i() const
{
    return track_i_;
}

void
Staff_performer::set_track( int& track_i_r )
{
    track_i_ = track_i_r++;
}
//</ugh>

