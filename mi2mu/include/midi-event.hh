//
// midi-event.hh -- declare midi_event
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_EVENT_HH
#define MIDI_EVENT_HH


// should these:
// * be Midi_items
// * be Voice_elements/requests
// * get a name-change
// ?

/// (midi_event)
class Midi_event {
public:
	Midi_event();
	
	virtual String mudela_str( bool command_mode_bo ) = 0;
	void output_mudela( Lily_stream& lily_stream_r, bool command_mode_bo );
	virtual Moment mom();
};

class Midi_key : public Midi_event {
public:
	Midi_key( int accidentals_i, int minor_i );

	String notename_str( int pitch_i );
	virtual String mudela_str( bool command_mode_bo );

private:
	int accidentals_i_;
	int minor_i_;
	int key_i_;
};

class Midi_note : public Midi_event {
public:
	int const c0_pitch_i_c_ = 60;

	Midi_note( String name_str, Duration dur );
	virtual Moment mom();
	virtual String mudela_str( bool command_mode_bo );
	
private:
	bool const simple_plet_bo_ = false;
	Duration dur_;
	String name_str_;
};

class Midi_tempo : public Midi_event {
public:
	Midi_tempo( int useconds_per_4_i );

	int get_tempo_i( Moment moment );
	virtual String mudela_str( bool command_mode_bo );
	int useconds_per_4_i();

private:
	int useconds_per_4_i_;
	Real seconds_per_1_f_;
};

class Midi_text : public Midi_event {
public:
	enum Type { 
		TEXT = 1, COPYRIGHT, TRACK_NAME, INSTRUMENT_NAME, LYRIC, 
		MARKER, CUE_POINT
	};
	Midi_text( Midi_text::Type type,  String str );
	virtual String mudela_str( bool command_mode_bo );
private:
	Type type_;
	String text_str_;
};

class Midi_time : public Midi_event {
public:
	Midi_time( int num_i, int den_i, int division_4_i, int count_32_i );

	Duration i2_dur( int time_i, int division_1_i );
	int clocks_1_i();
	int den_i();
	int num_i();
	virtual String mudela_str( bool command_mode_bo );
	Moment bar_mom();

private:
	Real sync_f_;
	Duration sync_dur_;
	int clocks_1_i_;
	int num_i_;
	int den_i_;
};

#endif // MIDI_EVENT_HH

