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
    DECLARE_MY_RUNTIME_TYPEINFO;

    Staff_performer();
    ~Staff_performer();

    String instrument_str();

protected:
    virtual void play_event( Midi_item* l );
    virtual void do_removal_processing();
    virtual void do_creation_processing();
    // <ugh>
    virtual void set_track( int& track_i_r );
    virtual int get_track_i() const;
    // </ugh>

private:
    void header();

    Moment midi_mom_;
    Midi_track* midi_track_p_;

    int track_i_;
};

#endif // STAFF_PERFORMER_HH
