/*
  midi-walker.cc -- implement Midi_walker

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
         Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "midi-walker.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "debug.hh"

Midi_note_event::Midi_note_event () 
{ 
  ignore_b_ = false;
}

int
compare (Midi_note_event const& left, Midi_note_event const& right)
{
  Moment m =   (left.key - right.key);

  if (m<0)
    return -1;
  else if (m > 0)
    return 1;
  else
    return 0;
}

Midi_walker::Midi_walker (Audio_staff* audio_staff_l, Midi_track* track_l)
{
  track_l_ = track_l;
  index_= 0;
  item_l_arr_l_ = &audio_staff_l->audio_item_l_arr_;

  last_mom_ = 0;
}

Midi_walker::~Midi_walker ()
{ 
  // ugh
  do_stop_notes (last_mom_ + Moment (Rational (10, 1)));
}

/**
  Find out if start_note event is needed, and do it if needed.
 */
void 
Midi_walker::do_start_note (Midi_note* note_p)
{
  Audio_item* ptr = (*item_l_arr_l_)[index_];
  Moment stop_mom = note_p->length_mom () + ptr->audio_column_l_->at_mom ();

  bool play_start = true;
  for (int i=0; i < stop_note_queue.size (); i++) 
    {
      /* if this pith already in queue */
      if (stop_note_queue[i].val->pitch_i () == note_p->pitch_i ()) 
	{
	  if (stop_note_queue[i].key < stop_mom)
	    {
	      /* let stopnote in queue be ignored,
	       new stop note wins */
	      stop_note_queue[i].ignore_b_ = true;
	      /* don't replay start note, */
	      play_start = false;
	      break;
	    }
	  else
	    {
	      /* skip this stopnote,
		 don't play the start note */
	      delete note_p;
	      note_p = 0;
	      break;
	  }
	}
    }

  if (note_p)
    {
      Midi_note_event e;
      e.val = new Midi_note_off (note_p);
      e.key = stop_mom;
      stop_note_queue.insert (e);

      if (play_start)
	output_event (ptr->audio_column_l_->at_mom (), note_p);
    }
}

/**
  Output note events for all notes which end before #max_mom#
 */
void
Midi_walker::do_stop_notes (Moment max_mom)
{
  while (stop_note_queue.size () && stop_note_queue.front ().key <= max_mom) 
    {
      Midi_note_event e = stop_note_queue.get ();
      if (e.ignore_b_)
	{
	  delete e.val;
	  continue;
	}
      
      Moment stop_mom = e.key;
      Midi_note* note_p = e.val;
	
      output_event (stop_mom, note_p);
    }
}

/** 
  Advance the track to #now#, output the item, and adjust current "moment". 
 */
void
Midi_walker::output_event (Moment now_mom, Midi_item* l)
{
  Moment delta_t = now_mom - last_mom_ ;
  last_mom_ = now_mom;

  /*
    this is not correct, but at least it doesn't crash when you
    start with graces
   */
  if (delta_t < Moment(0))
    {
      delta_t = Moment (0);
    }

  
  track_l_->add (delta_t, l);
}

void
Midi_walker::process ()
{
  Audio_item* audio_p = (*item_l_arr_l_)[index_];
  do_stop_notes (audio_p->audio_column_l_->at_mom ());

  if (Midi_item* midi_p = Midi_item::midi_p (audio_p))
    {
      midi_p->channel_i_ = track_l_->channel_i_;
      //midi_p->channel_i_ = track_l_->number_i_;
      if (Midi_note* note_p = dynamic_cast<Midi_note*> (midi_p))
	{
	  if (note_p->length_mom ().to_bool ())
	    do_start_note (note_p);
	}
      else
	output_event (audio_p->audio_column_l_->at_mom (), midi_p);
    }
}

bool
Midi_walker::ok () const
{
  return index_ <item_l_arr_l_->size ();
}

void
Midi_walker::operator ++ (int)
{
  assert (ok ());
  index_++;
}
