//
//  midiwalker.hh -- declare Midi_walker
//
//  (c) 1996,97 Han-Wen Nienhuys, Jan Nieuwenhuizen <jan@digicash.com>
//

#ifndef MIDIWALKER_HH
#define MIDIWALKER_HH

#include "proto.hh"
#include "grouping.hh"
#include "staffwalker.hh"

/// a simple walker which collects midi stuff, and then outputs
struct Midi_walker: Staff_walker {
//    Midi_stream* midi_stream_l_;
    /* *************** */
    virtual void process_requests();
    
    Midi_walker( Midi_staff* mstaff_l );
    Midi_column* mcol_l();
    Midi_staff* mstaff_l();
};


#endif // MIDIWALKER_HH


