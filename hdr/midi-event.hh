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
	virtual ~Midi_event();
	
	virtual String mudela_str(); // = 0;
	virtual void output_mudela( Lily_stream& lily_stream_r );
	virtual Moment mom();

protected:
	String mudela_str_;
};

class Midi_key : public Midi_event {
public:
	Midi_key( int accidentals_i, int minor );
	virtual ~Midi_key();

	String notename_str( int pitch_i );

private:
	int accidentals_i_;
	int minor_i_;
	int key_i_;
};

class Midi_note : public Midi_event {
    public:
	int const c0_pitch_i_c_ = 60;

        Midi_note( Midi_key* midi_key_l, Midi_time* midi_time_l, int division_1_i, int pitch_i, int time_i );
	virtual ~Midi_note();

	virtual Moment mom();
	
private:
	Duration dur_;
};

class Midi_tempo : public Midi_event {
public:
	Midi_tempo( int useconds_per_4_i );
	virtual ~Midi_tempo();

	int get_tempo_i( Moment moment );

private:
	int useconds_per_4_i_;
	Real seconds_per_1_f_;
};

class Midi_time : public Midi_event {
public:
	Midi_time( int num_i, int den_i, int division_4_i, int count_32_i );
	virtual ~Midi_time();

	Duration i2_dur( int time_i, int division_1_i );
	int clocks_1_i();

private:
	Real sync_f_;
	Duration sync_dur_;
	int clocks_1_i_;
	int num_i_;
	int den_i_;
};

#endif // MIDI_EVENT_HH

