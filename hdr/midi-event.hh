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
	
	String mudela_str();

protected:
	String mudela_str_;

private:
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
//	Midi_note( Midi_key* midi_key_l, Midi_tempo* midi_tempo_l, Midi_time* midi_time_l, int pitch_i, Real duration_f );
	Midi_note( Midi_key* midi_key_l, Midi_time* midi_time_l, int clocks_per_whole_i, int pitch_i, Real duration_f );
	virtual ~Midi_note();

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
	Midi_time( int num_i, int den_i, int clocks_i, int count_32_i );
	virtual ~Midi_time();

	String duration_str( int usecond24th_per_clock_i, Real delta_time_f );
	int whole_clocks_i();

private:
	int whole_clocks_i_;
	int num_i_;
	int den_i_;
};

#endif // MIDI_EVENT_HH

