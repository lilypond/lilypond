/*
  midi-walker.hh -- declare Midi_walker

  (c) 1996,97 Han-Wen Nienhuys, Jan Nieuwenhuizen <jan@digicash.com>
  */

#ifndef MIDIWALKER_HH
#define MIDIWALKER_HH

#include "proto.hh"
#include "grouping.hh"
#include "staff-walker.hh"
#include "pcursor.hh"
#include "pqueue.hh"


/**
  a simple walker which collects midi stuff, and then outputs.

  Should derive from Staff_walker
  */
class Midi_walker : public PCursor<Staff_column*> {
    Midi_track *track_l_;
    
    PQueue<Melodic_req*, Moment> stop_notes;
    Moment last_moment_;

    /* *************** */
    void do_stop_notes(Moment);
    void do_start_note(Note_req *note_l);
    void output_event(Midi_item&, Moment);
public:
    
    Midi_walker(Staff*, Midi_track*);
    void process_requests();
    ~Midi_walker();
};


#endif // MIDIWALKER_HH


