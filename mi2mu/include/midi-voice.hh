//
// midi-voice.hh -- declare midi_voice
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#ifndef MIDI_VOICE_HH
#define MIDI_VOICE_HH

/// (midi_voice)
class Midi_voice {
public:
	Midi_voice( Moment begin_mom );

	void add_event( Midi_event* midi_event_p );
	Moment begin_mom();
	Moment end_mom();

	String mudela_str( Moment to_mom, Moment to_mom, bool multiple_bo );
    // ARE you sure?              ^^             ^^  

private:
    int events_i_;
       Moment end_mom_;
       Moment begin_mom_;
	IPointer_list<Midi_event*> midi_event_p_list_;
};

#endif // MIDI_VOICE_HH

