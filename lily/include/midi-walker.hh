/*
  midi-walker.hh -- declare Midi_walker

  (c) 1996,  1997--1999 Han-Wen Nienhuys  <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef MIDI_WALKER_HH
#define MIDI_WALKER_HH

#include "proto.hh"
#include "pqueue.hh"
#include "lily-proto.hh"
#include "moment.hh"

struct Midi_note_event : PQueue_ent<Moment, Midi_note_off*>
{
  bool ignore_b_;
  Midi_note_event();
};

int compare (Midi_note_event const& left, Midi_note_event const& right);

/**
  walk audio and output midi
  */
class Midi_walker
{
public:
  Midi_walker (Audio_staff* audio_staff_l, Midi_track* midi_track_l);
  ~Midi_walker();

  void process();
  void operator ++(int);
  bool ok () const;
private:
  void do_start_note (Midi_note* note_p);
  void do_stop_notes (Moment now_mom);
  void output_event (Moment now_mom, Midi_item* l);

  Midi_track* track_l_;
  Audio_staff* staff_l_;
  int index_;
  Link_array<Audio_item> * item_l_arr_l_;
  PQueue<Midi_note_event> stop_note_queue;
  Moment last_mom_;
};


#endif // MIDI_WALKER_HH
