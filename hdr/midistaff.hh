/*
  midistaff.hh -- part of LilyPond

  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>
  */

#ifndef MIDI_STAFF_HH
#define MIDI_STAFF_HH

#include "staff.hh"

///(mstaff)
struct Midi_staff : Staff {
//    PStaff* pstaff_l_;

    Staff_column* create_col();
    virtual void set_output(PScore *);
    void midi( Midi_stream* midi_stream_l, int track_i );
    virtual Staff_walker *get_walker_p();
    Midi_staff();
};

#endif // MIDI_STAFF_HH





