/*
  midiwalker.cc -- implement Midi_walker

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>, Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "musicalrequest.hh"
#include "pscore.hh"
#include "staff.hh"
#include "midiwalker.hh"
#include "midiitem.hh"
#include "midistream.hh"
#include "debug.hh"
#include "staffcolumn.hh"

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
	
	Moment delta_t = stop_moment-last_moment_ ;
	last_moment_ += delta_t;
	track_l_->add(delta_t, &note );
    }
}

void
Midi_walker::process_requests()
{
    do_stop_notes(ptr()->when());
    for ( int i = 0; i < ptr()->musicalreq_l_arr_.size(); i++ )  {

	Rhythmic_req *n = ptr()->musicalreq_l_arr_[i]->rhythmic();
	if ( !n || !(n->note() || n->rest()) )
	    continue;
	
	Midi_note note(n->melodic(), track_l_->number_i_, true);
	stop_notes.enter(n->melodic(), n->duration() + ptr()->when() );
	Moment dt = 0;
	track_l_->add(dt, &note);
    }
}

Midi_walker::~Midi_walker()
{
    do_stop_notes( last_moment_ + Moment(10,1)); // ugh
}
