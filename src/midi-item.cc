//
// midi-item.cc
//
// source file of the LilyPond music typesetter
//
// (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <limits.h>
#include "plist.hh"
#include "pcol.hh"
#include "debug.hh"
#include "misc.hh"
#include "request.hh"
#include "musicalrequest.hh"
#include "voice.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "string-convert.hh"

Midi_chunk::Midi_chunk()
{
}

void
Midi_chunk::add( String str )
{
    data_str_ += str;
}

void
Midi_chunk::set( String header_str, String data_str, String footer_str )
{
    data_str_ = data_str;
    footer_str_ = footer_str;
    header_str_ = header_str;
}
  
String
Midi_chunk::str()
{
    String str = header_str_;
    String length_str = String_convert::i2hex_str( data_str_.length_i() + footer_str_.length_i(), 8, '0' );
    length_str = String_convert::hex2bin_str( length_str );
    str += length_str;
    str += data_str_;
    str += footer_str_;
    return str;
}

Midi_duration::Midi_duration( Real seconds_f )
{
    seconds_f_ = seconds_f;
}

String
Midi_duration::str()
{
    return String( "<duration: " ) + String( seconds_f_ ) + ">";
}

Midi_header::Midi_header( int format_i, int tracks_i, int clocks_per_4_i )
{
    String str;
	
    String format_str = String_convert::i2hex_str( format_i, 4, '0' );
    str += String_convert::hex2bin_str( format_str );
	
    String tracks_str = String_convert::i2hex_str( tracks_i, 4, '0' );
    str += String_convert::hex2bin_str( tracks_str );

    String tempo_str = String_convert::i2hex_str( clocks_per_4_i, 4, '0' );
    str += String_convert::hex2bin_str( tempo_str );

    set( "MThd", str, "" );
}

String
Midi_item::i2varint_str( int i )
{
    int buffer_i = i & 0x7f;
    while ( (i >>= 7) > 0 ) {
	buffer_i <<= 8;
	buffer_i |= 0x80;
	buffer_i += (i & 0x7f);
    }

    String str;
    while ( 1 ) {
	str += (char)buffer_i;
	if ( buffer_i & 0x80 )
	    buffer_i >>= 8;
	else
	    break;
    }
    return str;
}

void 
Midi_item::output_midi( Midi_stream& midi_stream_r )
{
    midi_stream_r << str();
}


Midi_note::Midi_note( Melodic_req* melreq_l, int channel_i, bool on_bo  )
{
    assert(melreq_l);
    pitch_i_ = melreq_l->pitch() + c0_pitch_i_c_;   
    channel_i_ = channel_i;

    // poor man-s staff dynamics:
    dynamic_byte_ =  (melreq_l)? 0x64 - 0x10 * channel_i_:0;
    on_b_ = on_bo;
}

String
Midi_note::str()
{
    if ( pitch_i_ != INT_MAX ) {
	Byte status_byte = ( on_b_ ? 0x90 : 0x80 ) + channel_i_;
	String str = String( (char)status_byte );
	str += (char)pitch_i_;
	// poor man-s staff dynamics:
	str += (char)dynamic_byte_;
	return str;
    }
    return String( "" );
}

Midi_tempo::Midi_tempo( int tempo_i )
{
    tempo_i_ = tempo_i;
}

String
Midi_tempo::str()
{
    int useconds_per_4_i = 60 * (int)1e6 / tempo_i_;
    String str = "ff5103";
    str += String_convert::i2hex_str( useconds_per_4_i, 6, '0' );
    return String_convert::hex2bin_str( str );
}

Midi_track::Midi_track( int number_i )
{
//                4D 54 72 6B     MTrk
//                00 00 00 3B     chunk length (59)
//        00      FF 58 04 04 02 18 08    time signature
//        00      FF 51 03 07 A1 20       tempo
 
// FF 59 02 sf mi  Key Signature
//         sf = -7:  7 flats
//         sf = -1:  1 flat
//         sf = 0:  key of C
//         sf = 1:  1 sharp
//         sf = 7: 7 sharps
//         mi = 0:  major key
//         mi = 1:  minor key

    number_i_ = number_i;
	
    char const* data_ch_c_l = "00" "ff58" "0404" "0218" "08"
//	"00" "ff51" "0307" "a120"
// why a key at all, in midi?
// key: C
	"00" "ff59" "02" "00" "00"
// key: F (scsii-menuetto)
//				  "00" "ff59" "02" "ff" "00"
	;

    String data_str;
    // only for format 0 (currently using format 1)?
    data_str += String_convert::hex2bin_str( data_ch_c_l );

    char const* footer_ch_c_l = "00" "ff2f" "00";
    String footer_str = String_convert::hex2bin_str( footer_ch_c_l );

    set( "MTrk", data_str, footer_str );
}

void 
Midi_track::add( int delta_time_i, String event_str )
{
    assert(delta_time_i >= 0);
    Midi_chunk::add( i2varint_str( delta_time_i ) + event_str );
}

void 
Midi_track::add( Moment delta_time_moment, Midi_item* mitem_l )
{
    // use convention of 384 clocks per 4
    // use Duration_convert
    int delta_time_i = delta_time_moment * Moment( 384 ) / Moment( 1, 4 );
    add( delta_time_i, mitem_l->str() );
}

