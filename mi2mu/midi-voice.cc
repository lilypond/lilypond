//
// midi-voice.cc -- implement midi_voice
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "mi2mu.hh"

Midi_voice::Midi_voice( Moment begin_mom )
{
	begin_mom_ = begin_mom;
	end_mom_ = begin_mom;
	events_i_ = 0;
}

void
Midi_voice::add_event( Midi_event* midi_event_p )
{
#ifdef MEVENT_LIST
	midi_event_p_list_.bottom().add( midi_event_p );
#else
	midi_event_p_array_.push( midi_event_p );
#endif
}

Moment 
Midi_voice::begin_mom()
{
	return begin_mom_;
}

Moment 
Midi_voice::end_mom()
{
#ifdef MEVENT_LIST
//	if ( events_i_ == midi_event_p_list_.length_i() )
	if ( events_i_ == midi_event_p_list_.size() )
		return end_mom_;
	Moment now_mom = begin_mom_;
	tor( DEBUG_ver ) << now_mom << ", ";
	for ( PCursor<Midi_event*> i( midi_event_p_list_.top() ); i.ok(); i++ ) {
		tor( DEBUG_ver ) << now_mom << ", ";
		now_mom += i->mom();
	}
	tor( DEBUG_ver ) << endl;
	end_mom_ = now_mom;
//	events_i_ = midi_event_p_list_.length_i();
	events_i_ = midi_event_p_list_.size();
	return end_mom_;
#else
	if ( events_i_ == midi_event_p_array_.size() )
		return end_mom_;
	Moment now_mom = begin_mom_;
	tor( DEBUG_ver ) << now_mom << ", ";
	for ( int i = 0; i < midi_event_p_array_.size(); i++ ) {
		tor( DEBUG_ver ) << now_mom << ", ";
		now_mom += midi_event_p_array_[ i ]->mom();
	}
	tor( DEBUG_ver ) << endl;
	end_mom_ = now_mom;
	events_i_ = midi_event_p_array_.size();
	return end_mom_;
#endif
}

String 
Midi_voice::mudela_str( Moment from_mom, Moment to_mom, bool multiple_bo )
{
	String str;

//	if ( begin_mom() >= to_mom )
	if ( begin_mom() > to_mom )
		return "";
//	if ( end_mom() <= from_mom )
	if ( end_mom() < from_mom )
		return "";
	
	Moment now_mom = begin_mom();
#ifdef MEVENT_LIST
	PCursor<Midi_event*> i( midi_event_p_list_.top() );
	for ( ; i.ok() && now_mom < from_mom ; i++ )
		now_mom += i->mom();
	
	for ( ; i.ok() && now_mom < to_mom ; i++ ) {
		now_mom += i->mom();
		str += i->mudela_str( false ) + " ";
	}
#else
	int i = 0;
	for ( ; i < midi_event_p_array_.size() && now_mom < from_mom ; i++ )
		now_mom += midi_event_p_array_[ i ]->mom();
	
	for ( ; i < midi_event_p_array_.size() && now_mom < to_mom ; i++ ) {
		now_mom += midi_event_p_array_[ i ]->mom();
		str += midi_event_p_array_[ i ]->mudela_str( false ) + " ";
	}
#endif
	
	if ( str.length_i() && multiple_bo )
		str = "{ " + str + "} ";
	return str;
}

