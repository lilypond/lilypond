//
// midi-stream.cc
//
// source file of the LilyPond music typesetter
//
// (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <fstream.h>
#include <time.h>
#include "main.hh"
#include "misc.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "debug.hh"
#include "string-convert.hh"

Midi_stream::Midi_stream( String filename_str, int tracks_i, int clocks_per_4_i ) 
{
    filename_str_ = filename_str;
    tracks_i_ = tracks_i;
    clocks_per_4_i_ = clocks_per_4_i;
    os_p_ = 0;
    open();
    header();
}

Midi_stream::~Midi_stream()
{
    delete os_p_;
}

Midi_stream&
Midi_stream::operator <<( String str )
{
    // still debugging...
    if ( check_debug )
	str = String_convert::bin2hex_str( str );
    // string now 1.0.26-2 handles binary streaming
    *os_p_ << str;
    return *this;
}

Midi_stream&
Midi_stream::operator <<( Midi_item& mitem_r )
{
    mitem_r.output_midi( *this );
    if ( check_debug )
        *os_p_ << "\n";
    return *this;
}

Midi_stream&
Midi_stream::operator <<( int i )
{
    // output binary string ourselves
    *this << Midi_item::i2varint_str( i );
    return *this;
}

void
Midi_stream::header()
{
//    *os_p_ << "% Creator: " << get_version();
//    *os_p_ << "% Automatically generated, at ";
//    time_t t(time(0));
//    *os_p_ << ctime(&t);

//                4D 54 68 64     MThd
//    String str = "MThd";
//                00 00 00 06     chunk length
//                00 01   format 1
//                00 01   one track
//                00 60   96 per quarter-note

//    char const ch_c_l = "0000" "0006" "0001" "0001" "0060";
//    str += String_convert::hex2bin_str( ch_c_l );
//    *os_p_ << str;

//      *this << Midi_header( 1, 1, tempo_i_ );
      *this << Midi_header( 1, tracks_i_, clocks_per_4_i_ );
}

void
Midi_stream::open()
{
    os_p_ = new ofstream( filename_str_ );
    if ( !*os_p_ )
	error ("can't open `" + filename_str_ + "\'" );
}
