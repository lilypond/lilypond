/*
  template8.cc -- instantiate audio List classes

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "midi-item.hh"
#include "pcursor.tcc"
#include "plist.tcc"


template POINTERLIST_INSTANTIATE(Audio_element);
template POINTERLIST_INSTANTIATE(Audio_column);
template POINTERLIST_INSTANTIATE(Midi_event);
