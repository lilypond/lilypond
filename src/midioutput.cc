/*
  midioutput.cc -- implement Midi_output

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>, Jan Nieuwehuizen <jan@digicash.com> 
*/

#include "plist.hh"
#include "debug.hh"
#include "score.hh"
#include "staff.hh"
#include "midistream.hh"
#include "mididef.hh"
#include "midioutput.hh"
#include "midiwalker.hh"
#include "midiitem.hh"
#include "staffcolumn.hh"
#include "musicalrequest.hh"


Midi_output:: Midi_output(Score* score_l, Midi_def* midi_l )
{
    midi_l_ = midi_l;

    Midi_stream midi_stream(midi_l->outfile_str_,
			    score_l->staffs_.size(),
			    384 );
// oeps, not tempo, but clocks per 4 (384 convention)
// must set tempo in tempo request
//			    midi_l->get_tempo_i(Moment(1, 4)));

    midi_stream_l_ = &midi_stream;
    int track_i=0;

    for (iter_top(score_l->staffs_,i); i.ok(); i++) {
	do_staff(i, track_i++);
    }

}

void
Midi_output::do_staff(Staff*st_l,int track_i)
{
    Midi_track midi_track( track_i );
    Midi_tempo midi_tempo( midi_l_->get_tempo_i( Moment( 1, 4 ) ) );
    midi_track.add( Moment( 0.0 ), &midi_tempo );
    for (Midi_walker w (st_l, &midi_track); w.ok(); w++)
	w.process_requests();

    *midi_stream_l_  << midi_track;
}  
