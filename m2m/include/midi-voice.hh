//
// midi-voice.hh -- declare midi_voice
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

/// (midi_voice)
#ifndef MIDI_VOICE_HH
#define MIDI_VOICE_HH

class Midi_voice {
public:
	Midi_voice( Moment begin_mom );

	void add_event( Midi_event* midi_event_p );
	Moment begin_mom();
	Moment end_mom();

	String mudela_str( Moment from_mom, Moment to_mom, bool multiple_bo );

private:
	Moment begin_mom_;
	IPointerList<Midi_event*> midi_event_p_list_;
};

#endif // MIDI_VOICE_HH

