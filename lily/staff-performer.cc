/*
  staff-performer.cc -- implement Staff_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#if 0

#include "staff-performer.hh"

String 
Staff_performer::instrument_str() 
{ 
    return Translator::id_str_; 
}

void 
Staff_performer::play_event( Midi_item i )
{
    i.track_i_ = midi_track_i_;
    Performer::play_event( i );
}

#endif 
