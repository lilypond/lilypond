#include "musicalrequest.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "staff.hh"
#include "midistaff.hh"
#include "midicolumn.hh"
#include "midiwalker.hh"
#include "midiitem.hh"
#include "midistream.hh"

Midi_staff::Midi_staff()
{
}

Staff_column*
Midi_staff::create_col()
{
	return new Midi_column(this);
}

Staff_walker*
Midi_staff::get_walker_p()
{
	return new Midi_walker(this);
}

void
Midi_staff::midi( Midi_stream* midi_stream_l, int track_i )
{
	Midi_track midi_track( track_i );
	Midi_column* last_mcol_l = 0;
	for ( Midi_walker w( this ); w.ok(); w++ ) {
		Midi_column* mcol_l = (Midi_column*)*w;
		if ( last_mcol_l )
			last_mcol_l->note_off( &midi_track, w->when() );
	    	mcol_l->note_on( &midi_track );
		last_mcol_l = mcol_l;
	}
	if ( last_mcol_l )
		last_mcol_l->note_off( &midi_track, last() );
	*midi_stream_l << midi_track;
}

void
Midi_staff::set_output(PScore*pscore_l)
{
	//i don-t want no pscore!
}

