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
	name_str_ = track_name_str;
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
Midi_track::add_begin_at( Pointer_list<Midi_voice*>& open_voices_r, Moment mom )
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

String
Midi_track::name_str()
{
	if ( name_str_.length_i() )
		return name_str_;
	return String( "track" ) + String( number_i_ );
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
	/* 
	   columns to voices
	*/
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
	lily_stream_r << name_str() << " = \\melodic{";
	lily_stream_r.indent();
	lily_stream_r << "% midi copyright:" << copyright_str_;
	lily_stream_r.newline();
	lily_stream_r << "% instrument:" << instrument_str_;
	lily_stream_r.newline();

	int bar_i = 0;

	Pointer_list<Midi_voice*> open_voices;
	Moment now_mom = 0.0;
	Real now_f = now_mom;
	Real begin_f = 0;
	Real end_f = end_mom();
	Real then_f;

	/* 
	   now we step through time while writing all voices

	   we can only output time slices that have a constant
	   number of open voices; each begin or end of a voice
	   starts or ends a chord or multivoice

	   [todo]
	   voice defragmentation/concatenation could make this
	   lost blonder
	*/

	bool start_of_track_bo = true;

	/// ugh, avoid status track 0 full of rests...
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

		now_f = now_mom;
		begin_f = begin_mom;
		end_f = end_mom;
		then_f = then_mom;
		
// ugh, rests
// checking for no open voice does not work for initial rests.
// for some reason the voice is open, but does not procuce notes?
		if ( open_voices.size() > 1 )
			lily_stream_r << "< ";
		if ( start_of_track_bo ) {
			start_of_track_bo = false;
			String str;
			for ( PCursor<Midi_voice*> i( open_voices.top() ); i.ok(); i++ )
				lily_stream_r << i->mudela_str( now_mom, then_mom, open_voices.size() - 1 );
			if ( str.length_i() )
				lily_stream_r << str;
			else
				output_mudela_rest( lily_stream_r, now_mom, then_mom );
		}
		else {
			for ( PCursor<Midi_voice*> i( open_voices.top() ); i.ok(); i++ )
				lily_stream_r << i->mudela_str( now_mom, then_mom, open_voices.size() - 1 );
			if ( !open_voices.size() )
				output_mudela_rest( lily_stream_r, now_mom, then_mom );
		}
//		*lily_stream_r.os_p_ << flush;
			
		if ( open_voices.size() > 1 )
			lily_stream_r << "> ";
		remove_end_at( open_voices, then_mom );

		bar_i = check_end_bar_i( now_mom, bar_i );

		now_mom = then_mom;
	}
	bar_i++;
	tor( NORMAL_ver ) << '[' << bar_i << ']' << flush; 

	lily_stream_r.tnedni();
	lily_stream_r << "} % " << name_str();
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

	int begin_bar_i = (int)( now_mom / bar_mom ) + 1; 
	int end_bar_i = (int)( end_mom / bar_mom ) + 1;

	if ( end_bar_i == begin_bar_i ) {
		output_mudela_rest_remain( lily_stream_r, end_mom - begin_mom );
		return;
	}

	// multiple bars involved
	int bar_i = (int)( now_mom / bar_mom ) + 1;

	//fill current bar
	Moment begin_bar_mom = Moment( begin_bar_i - 1 ) * bar_mom;
	if ( now_mom > begin_bar_mom ) {
		int next_bar_i = (int)( now_mom / bar_mom ) + 2; 
		Moment next_bar_mom = Moment( next_bar_i - 1 ) * bar_mom;
		assert( next_bar_mom <= end_mom );

		Moment remain_mom = next_bar_mom - now_mom;
		if ( remain_mom > Moment( 0 ) ) {
			output_mudela_rest_remain( lily_stream_r, remain_mom );
			now_mom += remain_mom;
		}

		bar_i = check_end_bar_i( now_mom, bar_i );
	}

	// fill whole bars
	int count_i = end_bar_i - bar_i;
	for ( int i = 0; i < count_i; i++ ) {
		int begin_bar_i = check_begin_bar_i( now_mom, bar_i );
		if ( begin_bar_i )
			output_mudela_begin_bar( lily_stream_r, now_mom, begin_bar_i );
		lily_stream_r << "r1 ";
//		*lily_stream_r.os_p_ << flush;
		tor( NORMAL_ver ) << begin_bar_i << flush; 
		bar_i = check_end_bar_i( now_mom, bar_i );
		now_mom += bar_mom;
	}

	// use "int i" here, and gcc 2.7.2 hits internal compiler error
	int ii = check_begin_bar_i( now_mom, bar_i );
	if ( ii )
		output_mudela_begin_bar( lily_stream_r, now_mom, ii );

//	bar_i = check_end_bar_i( now_mom, bar_i );

	Moment remain_mom = end_mom - Moment( end_bar_i - 1 ) * bar_mom;
        if ( remain_mom > Moment( 0 ) ) {
		output_mudela_rest_remain( lily_stream_r, remain_mom );
		now_mom += remain_mom;
	}
	assert( now_mom == end_mom );
}

void
Midi_track::output_mudela_rest_remain( Lily_stream& lily_stream_r, Moment mom )
{
	if ( Duration_convert::no_quantify_b_s ) {
		Duration dur = Duration_convert::mom2_dur( mom );
		lily_stream_r << "r" << dur.str() << " ";
//		assert( mom == dur.mom() );
		assert( mom == dur.length() );
		return;
	}
		
	Duration dur = Duration_convert::mom2standardised_dur( mom );
	if ( dur.type_i_ )
		lily_stream_r << "r" << dur.str() << " ";
}

void
Midi_track::remove_end_at( Pointer_list<Midi_voice*>& open_voices_r, Moment mom )
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

