/*
  audio-staff.hh -- declare Audio_staff

  (c) 1996,  1997--1998 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef AUDIO_STAFF_HH
#define AUDIO_STAFF_HH

#include "proto.hh"
#include "plist.hh"
#include "lily-proto.hh"
#include "audio-element.hh"

struct Audio_staff : public Audio_element {
    void add (Audio_item*  l);
    void output (Midi_stream& midi_stream_r, int track_i);

    Link_list<Audio_item*> audio_item_l_list_;
    DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // AUDIO_STAFF_HH
