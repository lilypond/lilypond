/*
  mididef.hh -- declare 

  source file of the LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef MIDIDEF_HH
#define MIDIDEF_HH
#include "proto.hh"
#include "real.hh"
#include "string.hh"
#include "moment.hh"


/** 
 */
struct Midi_def {
    /// output file name
    String outfile_str_;

    /// duration of whole note
    Real whole_seconds_f_;

    Midi_def();
    Midi_def(Midi_def const& midi_c_r);
    ~Midi_def();

    Real duration_to_seconds_f(Moment);
    int get_tempo_i( Moment moment );
    void set_tempo( Moment moment, int count_per_minute_i );
    void print() const;
};

#endif // MIDIDEF_HH //

