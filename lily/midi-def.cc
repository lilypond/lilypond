//
// midi-def.cc -- implement midi output
//
// source file of the GNU LilyPond music typesetter
//
// (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <math.h>
#include "misc.hh"
#include "midi-def.hh"
#include "debug.hh"

// classes, alphasorted
//     statics
//     constructors
//     destructor
//     routines, alphasorted

// statics Midi_def
// ugh
int Midi_def::den_i_s = 4;
int Midi_def::num_i_s = 4;

Midi_def::Midi_def()
{
    set_tempo( Moment( 1, 4 ), 60 );
    outfile_str_ = "lelie.midi"; 
}

Midi_def::Midi_def( Midi_def const& midi_c_r )
{
    whole_seconds_f_ = midi_c_r.whole_seconds_f_;
    outfile_str_ = midi_c_r.outfile_str_;
}

Midi_def::~Midi_def()
{
}

Real
Midi_def::duration_to_seconds_f( Moment moment )
{
    if (!moment)
	return 0;
    
    return Moment( whole_seconds_f_ ) * moment;
}

int
Midi_def::get_tempo_i( Moment moment )
{
    return Moment( whole_seconds_f_ ) * Moment( 60 ) * moment;
}

void
Midi_def::print() const
{
#ifndef NPRINT
    mtor << "Midi {4/min: " << Real( 60 ) / ( whole_seconds_f_ * 4 );
    mtor << "out: " << outfile_str_;
    mtor << "}\n";
#endif
}

void
Midi_def::set_tempo( Moment moment, int count_per_minute_i )
{
    whole_seconds_f_ = Moment( count_per_minute_i ) / Moment( 60 ) / moment;
}
