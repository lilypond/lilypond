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

	void add_event( Moment mom, Midi_event* midi_event_p );
	Moment end_mom();
	String name_str();
	void output_mudela( Lily_stream& lily_stream_r );
	Moment next_begin_mom( Moment now_mom );
	Moment next_end_mom( Moment now_mom );
	void process();
	Track_column* tcol_l( Moment mom );

	String copyright_str_;
	String instrument_str_;
	String name_str_;

private:
	void add_begin_at( PointerList<Midi_voice*>& open_voices_r, Moment mom );
	Midi_voice* get_free_midi_voice_l( Moment mom );
	void remove_end_at( PointerList<Midi_voice*>& open_voices_r, Moment mom );
	IPointerList<Track_column*> tcol_p_list_;
	IPointerList<Midi_voice*> midi_voice_p_list_;
	int number_i_;

};

#endif // MIDI_TRACK_HH

