//
// midi-track.hh -- declare midi_track
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_TRACK_HH
#define MIDI_TRACK_HH

/// (midi_track)
class Midi_track {
public:
	Midi_track( int number_i, String copyright_str, String track_name_str, String instrument_str );
	~Midi_track();

	void add_event( Moment mom, Midi_event* midi_event_p );
	Moment end_mom();
	String name_str();
	void output_mudela( Lily_stream& lily_stream_r );
	Moment next_begin_mom( Moment now_mom );
	Moment next_end_mom( Moment now_mom );
	void process();
	void set_tempo( int useconds_i );
	void set_time( int num_i, int den_i, int clocks_i, int count_32_i );
	Track_column* tcol_l( Moment mom );

	String copyright_str_;
	String instrument_str_;
	String name_str_;
	Midi_tempo* midi_tempo_p_;
	Midi_time* midi_time_p_;
	int number_i_;

private:
#ifdef MVOICE_LIST
	void add_begin_at( Link_list<Midi_voice*>& open_voices_r, Moment mom );
#else
	void add_begin_at( Array<Midi_voice*>& open_voices_r, Moment mom );
#endif
	int check_begin_bar_i( Moment now_mom, int open_bar_i );
	int check_end_bar_i( Moment now_mom, int open_bar_i );
	Midi_voice* get_free_midi_voice_l( Moment mom );
#ifdef MVOICE_LIST
	void remove_end_at( Link_list<Midi_voice*>& open_voices_r, Moment mom );
#else
	void remove_end_at( Array<Midi_voice*>& open_voices_r, Moment mom );
#endif
	void output_mudela_begin_bar( Lily_stream& lily_stream_r, Moment now_mom, int bar_i );
	void output_mudela_rest( Lily_stream& lily_stream_r, Moment begin_mom, Moment end_mom );
	void output_mudela_rest_remain( Lily_stream& lily_stream_r, Moment mom );

#ifdef TCOL_LIST
	Pointer_list<Track_column*> tcol_p_list_;
#else
	Array<Track_column*> tcol_p_array_;
#endif
#ifdef MVOICE_LIST
	Pointer_list<Midi_voice*> midi_voice_p_list_;
#else
	Array<Midi_voice*> midi_voice_p_array_;
#endif
};

#endif // MIDI_TRACK_HH

