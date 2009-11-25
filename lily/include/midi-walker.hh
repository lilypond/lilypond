/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Han-Wen Nienhuys  <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MIDI_WALKER_HH
#define MIDI_WALKER_HH

#include "pqueue.hh"
#include "lily-proto.hh"
#include "moment.hh"

struct Midi_note_event : PQueue_ent<int, Midi_note *>
{
  bool ignore_;
  Midi_note_event ();
};

int compare (Midi_note_event const &left, Midi_note_event const &right);

/**
   walk audio and output midi
*/
class Midi_walker
{
public:
  Midi_walker (Audio_staff *audio_staff, Midi_track *midi_track,
	       int channel);
  ~Midi_walker ();

  void process ();
  void operator ++ (int);
  bool ok () const;
  void finalize ();
private:
  void do_start_note (Midi_note *note);
  void do_stop_notes (int);
  void output_event (int, Midi_item *l);
  Midi_item *get_midi (Audio_item*); 
  int channel_;
  Midi_track *track_;
  Audio_staff *staff_;
  vsize index_;
  vector<Audio_item*> items_;
  PQueue<Midi_note_event> stop_note_queue;
  int last_tick_;

  vector<Midi_item*> midi_events_;
};

#endif // MIDI_WALKER_HH
