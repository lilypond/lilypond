/*
  midi-walker.cc -- implement Midi_walker

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>, Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "command-request.hh"
#include "musical-request.hh"
#include "p-score.hh"
#include "staff.hh"
#include "midi-walker.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "debug.hh"
#include "staff-column.hh"

Midi_walker::Midi_walker(Staff *st_l, Midi_track* track_l)
    : PCursor<Staff_column*>(st_l->cols_)
{
    track_l_ = track_l;
    last_moment_= 0;
}

/**
  output notestop events for all notes which end before #max_moment#
 */
void
Midi_walker::do_stop_notes(Moment max_moment)
{
    while (stop_notes.size() && stop_notes.front().key <= max_moment) {
	Note_event  ent=stop_notes.get();
	if (ent.ignore_b_) 
	    continue;
	
	Moment stop_moment = ent.key;
	Melodic_req * req_l = ent.val;
	
	Midi_note note(req_l, track_l_->number_i_, false);
	output_event(note, stop_moment);
    }
}
/**
  Find out if start_note event is needed,  and do it if needed.
 */
void 
Midi_walker::do_start_note(Note_req*note_l)
{
    Moment stop = note_l->duration() + ptr()->when();
    for(int i=0; i < stop_notes.size(); i++) {
	if (stop_notes[i].val->melodic()->pitch() ==
	    note_l->pitch()) {
	    if ( stop_notes[i].key < stop){
		stop_notes[i].ignore_b_=true;
	    }
	    else
		return; // skip the stop note 
	}
    }
    Note_event e;
    e.val = note_l;
    e.key = stop;
    
    stop_notes.insert(e);
    
    Midi_note note(note_l, track_l_->number_i_, true);
    output_event(note, ptr()->when());
}


/** advance the track to #now#, output the item, and adjust current
  "moment".  */
void
Midi_walker::output_event(Midi_item &i, Moment now)
{
    Moment delta_t = now - last_moment_ ;
    last_moment_ += delta_t;
    track_l_->add(delta_t, &i );    
}

void
Midi_walker::process_requests()
{
    do_stop_notes(ptr()->when());

    for ( int i = 0; i < ptr()->commandreq_l_arr_.size(); i++ )  {
	Command_req *c_l = ptr()->commandreq_l_arr_[i]->command();
	Meter_change_req* meter_l = c_l->meterchange();
	if ( meter_l )
	    output_event( Midi_time( meter_l->beats_i_, meter_l->one_beat_i_, 18 ),  0 );
	Key_change_req* key_l = c_l->keychange();
	if ( key_l ) {
	    int sharps_i = key_l->sharps_i();
	    int flats_i = key_l->flats_i();
	    // midi cannot handle non-conventional keys
	    if ( !( flats_i && sharps_i ) )
		output_event( Midi_key( sharps_i - flats_i, key_l->minor_b() ), 0 );
	}
    }

    for ( int i = 0; i < ptr()->musicalreq_l_arr_.size(); i++ )  {
	Rhythmic_req *n = ptr()->musicalreq_l_arr_[i]->rhythmic();
	if ( !n)
	    continue;
	Note_req * note_l = n->note();
	if (!note_l)
	    continue;
	do_start_note(note_l);
    }
}

Midi_walker::~Midi_walker()
{
    do_stop_notes( last_moment_ + Moment(10,1)); // ugh
}


int
compare(Note_event const&e1, Note_event const&e2)
{
    return sign(e1.key - e2.key);
}
