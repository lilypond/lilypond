//
// midi-def.cc -- implement midi output
//
// source file of the GNU LilyPond music typesetter
//
// (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include <math.h>
#include "misc.hh"
#include "midi-def.hh"
#include "input-translator.hh"
#include "performer-group-performer.hh"
#include "assoc-iter.hh"

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
    outfile_str_ = "lelie.midi"; 
    itrans_p_ = 0;
    real_vars_p_ = new Assoc<String,Real>;
    // ugh
    set_tempo( Moment( 1, 4 ), 60 );
}

Midi_def::Midi_def( Midi_def const& s )
{
    whole_seconds_f_ = s.whole_seconds_f_;
    itrans_p_ = s.itrans_p_ ? new Input_translator( *s.itrans_p_ ) : 0;
    real_vars_p_ = new Assoc<String,Real> ( *s.real_vars_p_ );
    outfile_str_ = s.outfile_str_;
}

Midi_def::~Midi_def()
{
    delete itrans_p_;
    delete real_vars_p_;
}

Real
Midi_def::duration_to_seconds_f( Moment mom )
{
    if ( !mom )
	return 0;
    
    return Moment( whole_seconds_f_ ) * mom;
}

Global_translator*
Midi_def::get_global_translator_p() const
{
    return  itrans_p_->get_group_performer_p()->global_l();
}

Real
Midi_def::get_var( String s ) const
{
    if ( !real_vars_p_->elt_b( s ) )
	error ( "unknown midi variable `"  + s + "'" );
    return real_vars_p_->elem( s );
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
    mtor << "Midi {";
    mtor << "4/min: " << Real( 60 ) / ( whole_seconds_f_ * 4 );
    mtor << "out: " << outfile_str_;
    for (Assoc_iter<String,Real> i( *real_vars_p_ ); i.ok(); i++) {
	mtor << i.key() << "= " << i.val() << "\n";
    }
    mtor << "}\n";
#endif
}

void
Midi_def::set( Input_translator* itrans_p )
{
    delete itrans_p_;
    itrans_p_ = itrans_p;
}

void
Midi_def::set_tempo( Moment moment, int count_per_minute_i )
{
    whole_seconds_f_ = Moment( count_per_minute_i ) / Moment( 60 ) / moment;
}

void
Midi_def::set_var( String s, Real r )
{
   real_vars_p_->elem( s ) = r;
}

