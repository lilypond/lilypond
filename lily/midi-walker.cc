/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include "midi-walker.hh"

#include "audio-column.hh"
#include "audio-staff.hh"
#include "midi-item.hh"
#include "midi-chunk.hh"
#include "midi-stream.hh"
#include "warn.hh"

Midi_note_event::Midi_note_event ()
{
  ignore_ = false;
}

int
compare (Midi_note_event const &left, Midi_note_event const &right)
{
  Moment m = (left.key - right.key);

  if (m < 0)
    return -1;
  else if (m > 0)
    return 1;
  else
    return 0;
}

bool
audio_item_less (Audio_item * const a,
		 Audio_item * const b)
{
  return a->get_column ()->when_ <  b->get_column ()->when_;
}

Midi_walker::Midi_walker (Audio_staff *audio_staff, Midi_track *track,
			  int channel)
{
  channel_ = channel;
  track_ = track;
  index_ = 0;
  items_ = audio_staff->audio_items_;
  vector_sort (items_, audio_item_less);
  last_tick_ = 0;
}

Midi_walker::~Midi_walker ()
{
  junk_pointers (midi_events_);
}

void
Midi_walker::finalize ()
{
  do_stop_notes (INT_MAX);
}

/**
   Find out if start_note event is needed, and do it if needed.
*/
void
Midi_walker::do_start_note (Midi_note *note)
{
  Audio_item *ptr = items_[index_];
  assert (note->audio_ == ptr);
  int stop_ticks = int (moment_to_real (note->audio_->length_mom_) * Real (384 * 4))
    + ptr->audio_column_->ticks ();

  bool play_start = true;
  for (vsize i = 0; i < stop_note_queue.size (); i++)
    {
      /* if this pith already in queue */
      if (stop_note_queue[i].val->get_semitone_pitch ()
	  == note->get_semitone_pitch ())
	{
	  if (stop_note_queue[i].key < stop_ticks)
	    {
	      /* let stopnote in queue be ignored,
		 new stop note wins */
	      stop_note_queue[i].ignore_ = true;

	      /* don't replay start note, */
	      play_start = false;
	      break;
	    }
	  else
	    {
	      /* skip this stopnote,
		 don't play the start note */
	      note = 0;
	      break;
	    }
	}
    }

  if (note)
    {
      Midi_note_event e;
      e.val = new Midi_note_off (note);

      midi_events_.push_back (e.val);
      e.key = int (stop_ticks);
      stop_note_queue.insert (e);

      if (play_start)
	output_event (ptr->audio_column_->ticks (), note);
    }
}

void
Midi_walker::do_stop_notes (int max_ticks)
{
  while (stop_note_queue.size () && stop_note_queue.front ().key <= max_ticks)
    {
      Midi_note_event e = stop_note_queue.get ();
      if (e.ignore_)
	{
	  continue;
	}

      int stop_ticks = e.key;
      Midi_note *note = e.val;

      output_event (stop_ticks, note);
    }
}

void
Midi_walker::output_event (int now_ticks, Midi_item *l)
{
  int delta_ticks = now_ticks - last_tick_;
  last_tick_ = now_ticks;

  /*
    this is not correct, but at least it doesn't crash when you
    start with graces
  */
  if (delta_ticks < 0)
    {
      programming_error ("Going back in MIDI time.");
      delta_ticks = 0;
    }

  track_->add (delta_ticks, l);
}

void
Midi_walker::process ()
{
  Audio_item *audio = items_[index_];
  do_stop_notes (audio->audio_column_->ticks ());

  if (Midi_item *midi = get_midi (audio))
    {
      if (Midi_channel_item *mci = dynamic_cast<Midi_channel_item*> (midi))
	mci->channel_ = channel_;
      
      if (Midi_note *note = dynamic_cast<Midi_note *> (midi))
	{
	  if (note->audio_->length_mom_.to_bool ())
	    do_start_note (note);
	}
      else
	output_event (audio->audio_column_->ticks (), midi);
    }
}

Midi_item*
Midi_walker::get_midi (Audio_item *i)
{
  Midi_item *mi = Midi_item::get_midi (i);
  midi_events_.push_back (mi);
  return mi;
}

bool
Midi_walker::ok () const
{
  return index_ < items_.size ();
}

void
Midi_walker::operator ++ (int)
{
  assert (ok ());
  index_++;
}
