#include "musicalrequest.hh"
#include "voice.hh"
#include "staffwalker.hh"
#include "debug.hh"
#include "staff.hh"
#include "midistaff.hh"
#include "midicolumn.hh"
#include "midiitem.hh"
#include "sccol.hh" 
#include "pscore.hh"
#include "main.hh"

Midi_column::Midi_column(Midi_staff* mstaff_l)
{
	mstaff_l_ = mstaff_l;
}

void
Midi_column::setup_one_request(Request*req_l)
{
	if ( req_l->melodic() ) 
	 	melreq_l_array_.push( req_l->melodic() );   
}

void
Midi_column::note_off( Midi_track* midi_track_l, Moment next )
{
	Moment delta_t = next - when();
	for ( int i = 0; i < melreq_l_array_.size(); i++ )  {
		Midi_note midi_note( melreq_l_array_[ i ], midi_track_l->number_i_, false );
		midi_track_l->add( delta_t, &midi_note );
		delta_t = 0.0;
	}
}

void
Midi_column::note_on( Midi_track* midi_track_l )
{
	Moment delta_t = 0.0;
	for ( int i = 0; i < melreq_l_array_.size(); i++ )  {
		Midi_note midi_note( melreq_l_array_[ i ], midi_track_l->number_i_, true );
		midi_track_l->add( delta_t, &midi_note );
	}
}
