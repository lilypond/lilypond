/*
  staff-performer.hh -- declare Staff_performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef STAFF_PERFORMER_HH
#define STAFF_PERFORMER_HH

#include "performer-group-performer.hh"

class Staff_performer : public Performer_group_performer 
{
    int midi_track_i_;

public:
    String instrument_str()
    virtual void play_event( Midi_item i );
};

#endif // STAFF_PERFORMER_HH
