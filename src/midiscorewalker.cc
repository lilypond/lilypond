/*
  midiscorewalker.cc -- implement Midi_score_walker

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>, Jan Nieuwehuizen <jan@digicash.com> 
*/
#include "plist.hh"
#include "debug.hh"
#include "score.hh"
#include "staffwalker.hh"
#include "staff.hh"
#include "sccol.hh"
#include "midistream.hh"
#include "midiscorewalker.hh"

Midi_score_walker::Midi_score_walker( Score* score_l, Midi_stream* midi_stream_l )
    :Score_walker( score_l )
{
	midi_stream_l_ = midi_stream_l;
}

Midi_score_walker::~Midi_score_walker()
{
}

void
Midi_score_walker::process()
{
}

