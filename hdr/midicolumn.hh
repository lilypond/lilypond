//
// midicolumn.hh -- declare Midi_column
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>


#ifndef MIDI_COLUMN_HH
#define MIDI_COLUMN_HH

#include "key.hh"
#include "stcol.hh"
#include "staff.hh"

/// (mcol)
struct Midi_column : Staff_column {

	Array<Melodic_req*> melreq_l_array_;
	Midi_staff* mstaff_l_;
	
	void note_on( Midi_track* midi_track_l );
	void note_off( Midi_track* midi_track_l, Moment next );
	virtual void setup_one_request(Request*);

	Midi_column(Midi_staff*rs);
};

#endif // MIDI_COLUMN_HH

