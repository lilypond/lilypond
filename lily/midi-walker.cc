/*
  midi-walker.cc -- implement Midi_walker

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>, Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "musicalrequest.hh"
#include "pscore.hh"
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
    while (stop_notes.size() && stop_notes.front_idx() <= max_moment) {
	Moment stop_moment = stop_notes.front_idx();
	Melodic_req * req_l = stop_notes.get();
	
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
    Moment stop=note_l->duration() + ptr()->when();
    for(int i=0; i < stop_notes.size(); i++)
	if (stop_notes.value_arr_[i]->melodic()->pitch() ==
	    note_l->pitch()) {
	     if ( stop_notes.indices_arr_[i] < stop){
	     	 
		 stop_notes.del(i);
		 return ;  // removing this gives a feature ( ${c2 c4}$ output correctly)
	     }
	     else
		 return; // skip the stop note 
	     break;// do the stop note	  	     
	}
    
    stop_notes.enter(note_l,  stop);
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
