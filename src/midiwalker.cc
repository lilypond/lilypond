/*
  midiwalker.cc -- implement Midi_walker

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>, Jan Nieuwenhuizen <jan@digicash.com>
*/

#include "misc.hh"
#include "musicalrequest.hh"
#include "voice.hh"
#include "pscore.hh"
#include "staff.hh"
#include "midicolumn.hh"
#include "midistaff.hh"
#include "midiwalker.hh"
#include "debug.hh"
#include "midiitem.hh"
#include "midistream.hh"

Midi_walker::Midi_walker( Midi_staff* mstaff_l )
    : Staff_walker( mstaff_l, 0 )
{
}

Midi_column*
Midi_walker::mcol_l()
{
	return (Midi_column*) *(*this);
}

Midi_staff*
Midi_walker::mstaff_l()
{
	return (Midi_staff*)staff_l_;
}

void
Midi_walker::process_requests()
{
	allow_break();
}

