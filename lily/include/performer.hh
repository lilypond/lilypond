/*
  performer.hh -- declare Performer

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef PERFORMER_HH
#define PERFORMER_HH

#include "lily-proto.hh"
#include "varray.hh"
#include "request.hh"
#include "score-elem-info.hh"
#include "staff-info.hh"

class Performer {
public:
    NAME_MEMBERS();
    Performer_group_performer* daddy_perf_l_;

    Performer();
    virtual ~Performer();

    void print() const;
    virtual void process_requests();
    virtual bool try_request( Request* req_l );

protected:
    virtual void do_print() const;
    virtual void play_event( Midi_item* l );
};

#include "global-performers.hh"

#endif // PERFORMER_HH
