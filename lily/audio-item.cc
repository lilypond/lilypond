/*
  audio-item.cc -- implement Audio items.

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "audio-item.hh"
#include "midi-item.hh"

#if 0
Audio_instrument::Audio_instrument( String instrument_str )
	: Audio_item( 0 )
{
}
#endif
                                      
Audio_item::Audio_item( Request* req_l )
{
    audio_column_l_ = 0;
    req_l_ = req_l;
}

Audio_key::Audio_key( Request* req_l )
	: Audio_item( req_l )
{
}

Midi_item*
Audio_key::midi_item_p()
{
    return new Midi_key( this );
}


Audio_note::Audio_note( Request* req_l, bool on_b )
	: Audio_item( req_l )
{
    on_b_ = on_b;
}

Midi_item*
Audio_note::midi_item_p()
{
    return new Midi_note( this );
}

Audio_tempo::Audio_tempo( int per_minute_4_i )
	: Audio_item( 0 )
{
    per_minute_4_i_ = per_minute_4_i;
}

Midi_item*
Audio_tempo::midi_item_p()
{
    return new Midi_tempo( this );
}

Audio_meter::Audio_meter( Request* req_l )
	: Audio_item( req_l )
{
}

Midi_item*
Audio_meter::midi_item_p()
{
    return new Midi_meter( this );
}

Audio_text::Audio_text( Audio_text::Type type, String text_str )
	: Audio_item( 0 )
{
	text_str_ = text_str;
	type_ = type;
}

Midi_item*
Audio_text::midi_item_p()
{
    return new Midi_text( this );
}

