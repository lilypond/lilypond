/*
  midi-def.hh -- declare Midi_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef MIDI_DEF_HH
#define MIDI_DEF_HH
#include "lily-proto.hh"
#include "real.hh"
#include "string.hh"
#include "moment.hh"


/** 
  definitions for midi output. Rather empty
 */
struct Midi_def {
    // ugh!
    static int den_i_s;
    static int num_i_s;

    /// output file name
    String outfile_str_;

    Input_translator* itrans_p_;

    /// duration of whole note
    Real whole_seconds_f_;

    Midi_def();
    Midi_def( Midi_def const& midi_c_r );
    ~Midi_def();

    Real duration_to_seconds_f(Moment);
    Global_translator* get_global_translator_p() const;
    int get_tempo_i( Moment moment );
    void print() const;
    void set( Input_translator* itrans_p );
    void set_tempo( Moment moment, int count_per_minute_i );
};

#endif // MIDI_DEF_HH

