//
// midi-track.cc -- implement Midi_track
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "mi2mu.hh"

Midi_track::Midi_track( int number_i, String copyright_str, String track_name_str, String instrument_str )
{
	number_i_ = number_i;
	copyright_str_ = copyright_str;
	instrument_str_ = instrument_str;
	if ( track_name_str.length_i() )
		name_str_ = track_name_str;
	else
		name_str_ = String( "track" ) + String( number_i_ );
	tcol_p_list_.bottom().add( new Track_column( Moment( 0 ) ) );
}

void
Midi_track::add_begin_at( PointerList<Midi_voice*>& open_voices_r, Moment mom )
{
	for ( PCursor<Midi_voice*> i( midi_voice_p_list_.top() ); i.ok(); i++ )
		if ( i->begin_mom() == mom ) {
			dtor << "open_voices (" << open_voices_r.size() << "): +1\n";
			open_voices_r.bottom().add( *i );
		}
}

void
Midi_track::add_event( Moment mom, Midi_event* midi_event_p )
{
	if ( ! midi_event_p )
		return;
	tcol_l( mom - midi_event_p->mom() )->add_event( midi_event_p );
}

// too much red tape?
String
Midi_track::name_str()
{
	return name_str_;
}

Moment
Midi_track::end_mom()
{
	// heu..
	Moment mom = 0.0;
	for ( PCursor<Midi_voice*> i( midi_voice_p_list_.top() ); i.ok(); i++ ) 
		mom = i->end_mom() >? mom;
	return mom;
}

Midi_voice*
Midi_track::get_free_midi_voice_l( Moment mom )
{
	for ( PCursor<Midi_voice*> i( midi_voice_p_list_.top() ); i.ok(); i++ )
		if ( i->end_mom() == mom )
			return *i;

	Midi_voice* midi_voice_p = new Midi_voice( mom );
	Midi_voice* midi_voice_l = midi_voice_p;
	midi_voice_p_list_.bottom().add( midi_voice_p );
	return midi_voice_l; 
}

Moment
Midi_track::next_begin_mom( Moment now_mom )
{
//	Moment begin_mom = Midi_track::end_mom() + 1;
	Moment begin_mom = Midi_track::end_mom();
	for ( PCursor<Midi_voice*> i( midi_voice_p_list_.top() ); i.ok(); i++ )
//		if ( i->begin_mom() >= now_mom )
		if ( i->begin_mom() > now_mom )
			begin_mom = begin_mom <? i->begin_mom();
	return begin_mom;
}

Moment
Midi_track::next_end_mom( Moment now_mom )
{
	Moment end_mom = Midi_track::end_mom();
	for ( PCursor<Midi_voice*> i( midi_voice_p_list_.top() ); i.ok(); i++ ) 
		if ( i->end_mom() > now_mom )
			end_mom = end_mom <? i->end_mom();
	return end_mom;
}

void
Midi_track::process()
{
	int bar_i = 1;
	Moment bar_mom = midi_parser_l_g->midi_time_p_->bar_mom();

	for ( PCursor<Track_column*> i( tcol_p_list_.top() ); i.ok(); i++ ) {
		int bars_i = (int)( i->mom() / bar_mom );
		if ( bars_i > bar_i )
			mtor << '[' << bar_i << flush; 
		while ( i->midi_event_p_list_.size() ) 
			// shit, where has the T* PCursor::remove() gone??
			// i don-t want to get and delete, 
			// i want to (re)move!
			// is it renamed: get vs add/insert ?? (put/remove :-)	
			get_free_midi_voice_l( i->mom() )->add_event( i->midi_event_p_list_.top().remove_p() );
		if ( bars_i > bar_i ) {
			bar_i++;
			mtor << ']' << flush; 
		}
	}

	dtor << "ends: " << endl;
	int n = 0;
	for ( PCursor<Midi_voice*> i( midi_voice_p_list_.top() ); i.ok(); i++ ) 
		vtor << "voice " << n++ << ": " << i->end_mom() << endl;
	dtor << ":sdne" << endl;
}


void
Midi_track::output_mudela( Lily_stream& lily_stream_r )
{
	lily_stream_r << name_str_ << " = music { $";
	lily_stream_r.indent();
	lily_stream_r << "% midi copyright:" << copyright_str_;
	lily_stream_r.newline();
	lily_stream_r << "% instrument:" << instrument_str_;
	lily_stream_r.newline();

	int bar_i = 1;
	Moment bar_mom = midi_parser_l_g->midi_time_p_->bar_mom();

	PointerList<Midi_voice*> open_voices;
	Moment now_mom = 0.0;
	Moment then_mom = 0.0;
	while ( now_mom < end_mom() ) {
		int bars_i = (int)( now_mom / bar_mom );
		if ( bars_i > bar_i )
			mtor << '[' << bar_i << flush; 
		add_begin_at( open_voices, now_mom );

		Moment begin_mom = next_begin_mom( now_mom ); 
		Moment end_mom = next_end_mom( now_mom ); 
		if ( ( begin_mom > now_mom ) && ( begin_mom < end_mom ) )
			then_mom = begin_mom;
		else 
			then_mom = end_mom;

		dtor << "begin: " << begin_mom << " end: " << end_mom << endl;
		dtor << "slice: " << now_mom << ", " << then_mom << endl;

		if ( open_voices.size() > 1 )
			lily_stream_r << "{ ";
		for ( PCursor<Midi_voice*> i( open_voices.top() ); i.ok(); i++ )
			lily_stream_r << i->mudela_str( now_mom, then_mom, open_voices.size() - 1 );
		if ( open_voices.size() > 1 )
			lily_stream_r << "} ";
		now_mom = then_mom;

		remove_end_at( open_voices, now_mom );
		if ( bars_i > bar_i ) {
			bar_i++;
			mtor << ']' << flush; 
		}
	}
	lily_stream_r.tnedni();
	lily_stream_r << "$} % " << name_str_;
	lily_stream_r.newline();
}

void
Midi_track::remove_end_at( PointerList<Midi_voice*>& open_voices_r, Moment mom )
{
	for ( PCursor<Midi_voice*> i( open_voices_r.top() ); i.ok(); i++ )
		if ( i->end_mom() <= mom ) {
			dtor << "open_voices (" << open_voices_r.size() << "): -1\n";
			i.remove_p();  // remove? // no delete; only a copy
			if ( !i.ok() )
				break;
		}
}

Track_column*
Midi_track::tcol_l( Moment mom )
{
	for ( PCursor<Track_column*> i( tcol_p_list_.top() ); i.ok(); i++ ) {
    		if ( i->mom() == mom )
			return *i;
		if ( i->mom() > mom ) {
			Track_column* tcol_p = new Track_column( mom );
			i.insert( tcol_p );
			return tcol_p;
		}
	}

	Track_column* tcol_p = new Track_column( mom );
	tcol_p_list_.bottom().add( tcol_p );
	return tcol_p;
}

