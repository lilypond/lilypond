/*
  midi-walker.hh -- declare Midi_walker

  (c) 1996,97 Han-Wen Nienhuys, Jan Nieuwenhuizen <jan@digicash.com>
  */

#ifndef MIDIWALKER_HH
#define MIDIWALKER_HH


#include "lily-proto.hh"

#if 0

// sketch. . .
class Midi_register {
};

class Midi_group_reg : public Midi_register, public Acceptor {
    
};

class Midi_staff_reg : public Midi_group_reg {
};


class Midi_note_reg : public Midi_register {
    
};

class Midi_voice_reg : public Midi_group_reg, public Interpreter {
    
};

class Midi_score_reg: public Midi_group_reg, public Global_acceptor {
};


#include "grouping.hh"
#include "staff-walker.hh"
#include "pcursor.hh"
#include "pqueue.hh"


struct Note_event : PQueue_ent<Moment,Melodic_req*>
{
    bool ignore_b_;
    Note_event() { 
	ignore_b_ = false;
    }
};

int compare(Note_event const&, Note_event const&);

/**
  a simple walker which collects midi stuff, and then outputs.

  Should derive from Staff_walker
  */
class Midi_walker : public PCursor<Staff_column*> 
{
    Midi_track *track_l_;
    
    PQueue< Note_event > stop_notes;

    Moment last_moment_;

    /* *************** */
    void do_stop_notes(Moment);
    void do_start_note(Note_req *note_l);
    void output_event(Midi_item&, Moment);
public:
    
    Midi_walker(Staff*, Midi_track*);
    void do_process_requests();
    ~Midi_walker();
};
#endif

#endif // MIDIWALKER_HH


