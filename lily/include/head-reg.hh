/*
  headreg.hh -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HEADREG_HH
#define HEADREG_HH
#include "register.hh"

struct Note_head_register : Request_register {
    Note_head* note_p_;
    Rhythmic_req * note_req_l_;
    
    /* *************** */
    Note_head_register();
    virtual bool try_request(Request *req_l) ;
    virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    NAME_MEMBERS(Note_head_register);
};


#endif // HEADREG_HH
