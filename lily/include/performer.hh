/*
  performer.hh -- declare Performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef PERFORMER_HH
#define PERFORMER_HH

#include "lily-proto.hh"

class Performer {
    Performer_group_performer* daddy_perf_l_;
    
public:
    NAME_MEMBERS();
    Performer();
    virtual ~Performer();

    void print() const;

    virtual void play_event( Midi_item i );
    virtual bool try_request( Request* req_l );
};

#include "global-performers.hh"

#endif // PERFORMER_HH
