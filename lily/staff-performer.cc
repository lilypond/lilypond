/*
  staff-performer.cc -- implement Staff_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */


#include "staff-performer.hh"
#include "translator.hh"
#include "input-translator.hh"
#include "debug.hh"

IMPLEMENT_STATIC_NAME(Staff_performer);
IMPLEMENT_IS_TYPE_B1(Staff_performer,Performer_group_performer);
ADD_THIS_PERFORMER(Staff_performer);

Staff_performer::Staff_performer()
{
}

Staff_performer::~Staff_performer()
{
}

String 
Staff_performer::instrument_str() 
{ 
    return Translator::id_str_; 
}

void 
Staff_performer::play_event( Midi_item* l )
{
//    l.track_i_ = midi_track_i_;
    Performer::play_event( l );
}

