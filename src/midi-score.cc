//
// midi-score.cc -- implement Midi_score
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>

#include "proto.hh"
#include "midi-track.hh"
#include "midi-score.hh"

Midi_score::Midi_score( int format_i, int tracks_i, int tempo_i )
{
	format_i_ = format_i;
	tracks_i_ = tracks_i;
	tempo_i_ = tempo_i;
}

Midi_score::~Midi_score()
{
}

void
Midi_score::add_track( Midi_track* midi_track_p )
{
}
