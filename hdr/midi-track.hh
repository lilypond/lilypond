//
// midi-track.hh -- declare midi_track
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_TRACK_HH
#define MIDI_TRACK_HH

/// (midi_track)
class Midi_track {
public:
	Midi_track();
	~Midi_track();

	void add_event( Midi_event* midi_event_p );

private:
};

#endif // MIDI_TRACK_HH

