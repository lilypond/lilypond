/*
  audio-staff.hh -- declare Audio_staff

  (c) 1996,  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef AUDIO_STAFF_HH
#define AUDIO_STAFF_HH

#include "proto.hh"
#include "parray.hh"
#include "lily-proto.hh"
#include "audio-element.hh"

struct Audio_staff : public Audio_element {
  void add_audio_item (Audio_item*  l);
  void output (Midi_stream& midi_stream_r, int track_i);

  Link_array<Audio_item> audio_item_l_arr_;
};

#endif // AUDIO_STAFF_HH
