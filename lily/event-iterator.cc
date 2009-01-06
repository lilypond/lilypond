/*
  event-chord-iterator.cc -- implement Event_chord_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "event-iterator.hh"

#include "context.hh"
#include "duration.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "warn.hh"

Event_iterator::Event_iterator ()
{
}

void
Event_iterator::construct_children ()
{
  descend_to_bottom_context ();
  Simple_music_iterator::construct_children ();
}

void
Event_iterator::process (Moment m)
{
  if (last_processed_mom_ < Moment (0))
    report_event (get_music ());

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Event_iterator);
