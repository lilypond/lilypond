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
	midi_time_p_ = new Midi_time( 4, 2, 24, 8 );
	midi_tempo_p_ = new Midi_tempo( 1000000 );
	tcol_p_list_.bottom().add( new Track_column( Moment( 0 ) ) );
}

Midi_track::~Midi_track()
{
	delete midi_time_p_;
	delete midi_tempo_p_;
}

void
Midi_track::add_begin_at( PointerList<Midi_voice*>& open_voices_r, Moment mom )
{
	for ( PCursor<Midi_voice*> i( midi_voice_p_list_.top() ); i.ok(); i++ )
		if ( i->begin_mom() == mom ) {
			tor( DEBUG_ver ) << "open_voices (" << open_voices_r.size() << "): +1\n";
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

int
Midi_track::check_begin_bar_i( Moment now_mom, int open_bar_i )
{
	Moment bar_mom = midi_time_p_->bar_mom();
	int bar_i = (int)( now_mom / bar_mom ) + 1;
	if ( bar_i > open_bar_i ) { 
		tor( NORMAL_ver ) << '[' << flush; 
		return bar_i;
	}
	return 0;
}

int
Midi_track::check_end_bar_i( Moment now_mom, int open_bar_i )
{
	Moment bar_mom = midi_time_p_->bar_mom();
	int bar_i = (int)( now_mom / bar_mom ) + 1;
	if ( bar_i > open_bar_i ) {
		tor( NORMAL_ver ) << ']' << flush; 
		return bar_i;
	}
	return open_bar_i;
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

// too much red tape?
String
Midi_track::name_str()
{
	return name_str_;
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
	for ( PCursor<Track_column*> i( tcol_p_list_.top() ); i.ok(); i++ ) {
		int begin_bar_i = check_begin_bar_i( i->mom(), bar_i );
		if ( begin_bar_i )
			tor( NORMAL_ver ) << begin_bar_i << flush; 
		while ( i->midi_event_p_list_.size() ) 
			get_free_midi_voice_l( i->mom() )->add_event( i->midi_event_p_list_.top().remove_p() );
		bar_i = check_end_bar_i( i->mom(), bar_i );
	}

	tor( DEBUG_ver ) << "ends: " << endl;
	int n = 0;
	for ( PCursor<Midi_voice*> i( midi_voice_p_list_.top() ); i.ok(); i++ ) 
		tor( VERBOSE_ver ) << "voice " << n++ << ": " << i->end_mom() << endl;
	tor( DEBUG_ver ) << ":sdne" << endl;
}


void
Midi_track::output_mudela( Lily_stream& lily_stream_r )
{
	lily_stream_r << name_str_ << " = \\melodic{";
	lily_stream_r.indent();
	lily_stream_r << "% midi copyright:" << copyright_str_;
	lily_stream_r.newline();
	lily_stream_r << "% instrument:" << instrument_str_;
	lily_stream_r.newline();

	int bar_i = 0;

	PointerList<Midi_voice*> open_voices;
	Moment now_mom = 0.0;
	/// ugh, avoid status track 0...
	while ( number_i_ && ( now_mom < end_mom() ) ) {
		int begin_bar_i = check_begin_bar_i( now_mom, bar_i );
		if ( begin_bar_i )
			output_mudela_begin_bar( lily_stream_r, now_mom, begin_bar_i );
		add_begin_at( open_voices, now_mom );

		Moment begin_mom = next_begin_mom( now_mom ); 

		if ( begin_bar_i )
			tor( NORMAL_ver ) << begin_bar_i << flush; 

		Moment end_mom = next_end_mom( now_mom ); 
		Moment then_mom = 0.0;
		if ( ( begin_mom > now_mom ) && ( begin_mom < end_mom ) )
			then_mom = begin_mom;
		else 
			then_mom = end_mom;

		tor( DEBUG_ver ) << "begin: " << begin_mom << " end: " << end_mom << endl;
		tor( DEBUG_ver ) << "slice: " << now_mom << ", " << then_mom << endl;

// rests, ugh
		String str;
//		if ( !open_voices.size() )
//			output_mudela_rest( lily_stream_r, now_mom, then_mom );
		if ( open_voices.size() > 1 )
			lily_stream_r << "< ";
		for ( PCursor<Midi_voice*> i( open_voices.top() ); i.ok(); i++ )
//			lily_stream_r << i->mudela_str( now_mom, then_mom, open_voices.size() - 1 );
			str += i->mudela_str( now_mom, then_mom, open_voices.size() - 1 );
		if ( str.length_i() )
			lily_stream_r << str;
		else
			output_mudela_rest( lily_stream_r, now_mom, then_mom );
			
		if ( open_voices.size() > 1 )
			lily_stream_r << "> ";
		remove_end_at( open_voices, then_mom );

		bar_i = check_end_bar_i( now_mom, bar_i );

		now_mom = then_mom;
	}
	bar_i++;
	tor( NORMAL_ver ) << '[' << bar_i << ']' << flush; 

	lily_stream_r.tnedni();
	lily_stream_r << "} % " << name_str_;
	lily_stream_r.newline();
}


void
Midi_track::output_mudela_begin_bar( Lily_stream& lily_stream_r, Moment now_mom, int bar_i )
{
	Moment bar_mom = midi_time_p_->bar_mom();
	Moment into_bar_mom = now_mom - Moment( bar_i - 1 ) * bar_mom;
	if ( bar_i > 1 ) {
		if ( !into_bar_mom )
			lily_stream_r << "|";
		lily_stream_r.newline();
	}
	lily_stream_r << "% " << String_convert::i2dec_str( bar_i, 0, ' ' );
	if ( into_bar_mom )
		lily_stream_r << ":" << Duration_convert::dur2_str( Duration_convert::mom2_dur( into_bar_mom ) );
	lily_stream_r.newline();
}


void 
Midi_track::output_mudela_rest( Lily_stream& lily_stream_r, Moment begin_mom, Moment end_mom )
{
	Moment bar_mom = midi_time_p_->bar_mom();
	Moment now_mom = begin_mom;
	int begin_bar_i =(int)( now_mom / bar_mom ) + 1; 
	Moment remain_mom = now_mom - Moment( begin_bar_i - 1 ) * bar_mom;
        if ( remain_mom > Moment( 0 ) )
		output_mudela_rest_remain( lily_stream_r, remain_mom );

	int end_bar_i = (int)( end_mom / bar_mom ) + 1;
	now_mom += remain_mom;

	int bar_i = (int)( now_mom / bar_mom ) + 1;
	bar_i = check_end_bar_i( now_mom, bar_i );
	for ( int i = 0; i < end_bar_i - begin_bar_i; i++ ) {
		int begin_bar_i = check_begin_bar_i( now_mom, bar_i );
		if ( begin_bar_i )
			output_mudela_begin_bar( lily_stream_r, now_mom, begin_bar_i );
		lily_stream_r << "r1 ";
		tor( NORMAL_ver ) << begin_bar_i << flush; 
		bar_i = check_end_bar_i( now_mom, bar_i );
		now_mom += bar_mom;
	}
	// use "int i" here, and gcc 2.7.2 hits internal compiler error
	int ii = check_begin_bar_i( now_mom, bar_i );
	if ( ii )
		output_mudela_begin_bar( lily_stream_r, now_mom, ii );
	bar_i = check_end_bar_i( now_mom, bar_i );

	remain_mom = end_mom - Moment( end_bar_i - 1 ) * bar_mom;
        if ( remain_mom > Moment( 0 ) )
		output_mudela_rest_remain( lily_stream_r, remain_mom );
}


void
Midi_track::output_mudela_rest_remain( Lily_stream& lily_stream_r, Moment mom )
{
	int type_i = 2;
	while ( mom > Moment( 0 ) ) {
		Duration dur( type_i );
		Moment type_mom = Duration_convert::dur2_mom( dur );
		int count_i = (int)( mom / type_mom ); 
		for( int i = 0; i < count_i; i++ )
			lily_stream_r << "r" << dur.str() << " ";
		type_i *= 2;
		mom -= Moment( count_i ) * type_mom;
		if ( Duration_convert::no_smaller_than_i_s
			&& ( type_i > Duration_convert::no_smaller_than_i_s ) ) 
			break;	
	}

}

void
Midi_track::remove_end_at( PointerList<Midi_voice*>& open_voices_r, Moment mom )
{
	for ( PCursor<Midi_voice*> i( open_voices_r.top() ); i.ok(); i++ )
//		if ( i->end_mom() == mom ) { }
		if ( i->end_mom() <= mom ) {
			tor( DEBUG_ver ) << "open_voices (" << open_voices_r.size() << "): -1\n";
			i.remove_p();
			if ( !i.ok() )
				break;
		}
}

void
Midi_track::set_tempo( int useconds_per_4_i )
{
	delete midi_tempo_p_;
	midi_tempo_p_ = new Midi_tempo( useconds_per_4_i );
}

void
Midi_track::set_time( int num_i, int den_i, int clocks_i, int count_32_i )
{
	delete midi_time_p_;
	midi_time_p_ = new Midi_time( num_i, den_i, clocks_i, count_32_i );
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

