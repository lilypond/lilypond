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
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();
    virtual void do_post_move_processing();
public:
    NAME_MEMBERS();
    Lyric_register();
};
#endif // LYRIC_REGISTER_HH
