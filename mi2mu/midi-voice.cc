//
// midi-voice.cc -- implement midi_voice
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "mi2mu.hh"

Midi_voice::Midi_voice( Moment begin_mom )
{
	begin_mom_ = begin_mom;
}

void
Midi_voice::add_event( Midi_event* midi_event_p )
{
	midi_event_p_list_.bottom().add( midi_event_p );
}

Moment 
Midi_voice::begin_mom()
{
	return begin_mom_;
}

Moment 
Midi_voice::end_mom()
{
	Moment now_mom = begin_mom_;
	tor( DEBUG_ver ) << now_mom << ", ";
	for ( PCursor<Midi_event*> i( midi_event_p_list_.top() ); i.ok(); i++ ) {
		tor( DEBUG_ver ) << now_mom << ", ";
		now_mom += i->mom();
	}
	tor( DEBUG_ver ) << endl;
	return now_mom;
}

String 
Midi_voice::mudela_str( Moment from_mom, Moment to_mom, bool multiple_bo )
{
	String str;

	if ( begin_mom() >= to_mom )
		return "";
	if ( end_mom() <= from_mom )
		return "";
	
	Moment now_mom = begin_mom();
	PCursor<Midi_event*> i( midi_event_p_list_.top() );
	for ( ; i.ok() && now_mom < from_mom ; i++ )
		now_mom += i->mom();
	
	for ( ; i.ok() && now_mom < to_mom ; i++ ) {
		now_mom += i->mom();
		str += i->mudela_str( false ) + " ";
	}
	
	if ( str.length_i() && multiple_bo )
		str = "{ " + str + "} ";
	return str;
}

