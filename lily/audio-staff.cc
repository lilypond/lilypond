/*
  audio-staff.cc -- implement Audio_staff

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "audio-staff.hh"

#include "midi-chunk.hh"
#include "midi-stream.hh"
#include "midi-walker.hh"

void
Audio_staff::add_audio_item (Audio_item *l)
{
  audio_items_.push_back (l);
}

Audio_staff::Audio_staff ()
{
  channel_ = -1; 
}

void
Audio_staff::output (Midi_stream &midi_stream, int channel)
{
  Midi_track midi_track;
  midi_track.number_ = channel;

  Midi_walker i (this, &midi_track, channel);
  for (; i.ok (); i++)
    i.process ();

  i.finalize ();
  
  midi_stream.write (midi_track);
}

