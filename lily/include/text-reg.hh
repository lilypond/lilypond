/*
  text-reg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef TEXTREG_HH
#define TEXTREG_HH
#include "register.hh"

struct Text_register : Request_register{
    Text_item * text_p_;
    Text_req * text_req_l_;
    int dir_i_;
    /* *************** */
    virtual void set_feature(Features );
    Text_register();
    virtual bool try_request(Request*);
    virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    NAME_MEMBERS(Text_register);
};

#endif // TEXTREG_HH
