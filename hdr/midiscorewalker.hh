//
//  midiscorewalker.hh -- declare Midi_score_walker
//
//  (c) 1996,97 Han-Wen Nienhuys, Jan Nieuwenhuizen <jan@digicash.com>
//

#ifndef MIDI_SCORE_WALKER_HH
#define MIDI_SCORE_WALKER_HH

#include "proto.hh"
#include "grouping.hh"
#include "scorewalker.hh"
#include "midiitem.hh"

/// a simple walker which collects midi stuff, and then outputs
struct Midi_score_walker : Score_walker {
    Midi_stream* midi_stream_l_;

    /* *************** */
    virtual void process();
    
    Midi_score_walker( Score* score_l, Midi_stream* midi_stream_l );
};


#endif // MIDI_SCORE_WALKER_HH


