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
    virtual void midi_output( Midi_stream* midi_stream_l_ );
    virtual void play_event( Midi_item* l );
    virtual void set_track( Midi_def* midi_l, int& track_i_r );

private:
    void header();

    Midi_def* midi_l_;
    Moment midi_mom_;
    int track_i_;
    Midi_track* midi_track_p_;
};

#endif // STAFF_PERFORMER_HH
