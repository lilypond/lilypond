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
public:
    NAME_MEMBERS();

    Staff_performer();
    ~Staff_performer();

    String instrument_str();

protected:
    virtual void play_event( Midi_item* l );

private:
    int midi_track_i_;
};

#endif // STAFF_PERFORMER_HH
