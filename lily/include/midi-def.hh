/*
  midi-def.hh -- declare Midi_def

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
*/


#ifndef MIDIDEF_HH
#define MIDIDEF_HH
#include "lily-proto.hh"
#include "real.hh"
#include "string.hh"
#include "moment.hh"


/** 
 */
struct Midi_def {
    // ugh!
    static int den_i_s;
    static int num_i_s;

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

