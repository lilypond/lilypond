//
// midi-track.hh -- declare midi_track
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_TRACK_HH
#define MIDI_TRACK_HH

/// (midi_track)
class Midi_track {
public:
	Midi_track( int track_i );
	~Midi_track();

	void add_event( Moment mom, Midi_event* midi_event_p );
	String name_str();
	void output_mudela( Lily_stream& lily_stream_r );
	Track_column* tcol_l( Moment mom );

private:
	IPointerList<Track_column*> tcol_p_list_;
	String name_str_;
};

#endif // MIDI_TRACK_HH

