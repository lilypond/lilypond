/*
  lyric-register.hh -- declare Lyric_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LYRIC_REGISTER_HH
#define LYRIC_REGISTER_HH
#include "register.hh"
#include "varray.hh"

#include "lily-proto.hh"

class Lyric_register : public Request_register {
    Array<Lyric_req*> lreq_arr_;
    virtual bool acceptable_request_b(Request*);
    virtual bool try_request(Request*);
    virtual void process_requests();
    virtual void post_move_processing();
public:
    NAME_MEMBERS();
    Lyric_register();
};
#endif // LYRIC_REGISTER_HH
